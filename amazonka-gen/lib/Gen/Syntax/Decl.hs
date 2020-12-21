-- |
-- Module      : Gen.Syntax.Decl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Gen.Syntax.Decl
  ( -- * Datatypes
    derivingD,
    patternD,
    recordD,
    fieldsD,
    sigSmartCtorD,
    funSmartCtorD,
    sigLensD,
    funLensD,

    -- * Service configuration
    sigServiceD,
    funServiceD,
    sigErrorD,
    funErrorD,
    sigWaiterD,
    funWaiterD,

    -- * Instances
    instancesD,
    classAWSRequestD,
    classAWSPagerD,
  )
where

import qualified Control.Comonad as Comonad
import qualified Control.Lens as Lens
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict.InsOrd as HashMap
import qualified Data.List as List
import qualified Data.Text as Text
import Gen.Prelude
import Gen.Protocol (Names (..))
import qualified Gen.Protocol as Protocol
import qualified Gen.Syntax.Exts as Exts
import Gen.Types
import Gen.Types.Field
import Gen.Types.Instance

-- Datatypes

-- The data declaration, deriving clauses, and fields are all rendered
-- separately since it's easier to interpserse the formatted documentation
-- using templating.

derivingD :: Bool -> [Derive] -> [Exts.Deriving]
derivingD isNewtype =
  Exts.derivingD isNewtype
    . map derivingStrategy

patternD :: Id -> Text -> [Exts.Deriving] -> Exts.Decl
patternD (typeId -> name) ctor =
  Exts.dataD
    name
    True
    [ Exts.recordD (name <> "'") [Exts.fieldD ctor (Exts.conT "Core.Text")]
    ]

recordD :: Id -> Bool -> Exts.Decl
recordD (typeId -> name) isNewtype =
  Exts.dataD
    name
    isNewtype
    [ Exts.conD (name <> "'") []
    ]
    []

fieldsD ::
  HasMetadata meta Identity =>
  meta ->
  Id ->
  [Field] ->
  [(Text, Exts.FieldDecl, Maybe Help)]
fieldsD meta name =
  map $ \field ->
    ( fieldAccessor field,
      Exts.fieldD (fieldAccessor field) (fieldT meta (Just (typeId name)) field),
      _refDocumentation (_fieldRef field)
    )

sigSmartCtorD :: HasMetadata meta Identity => meta -> Id -> [Field] -> Exts.Decl
sigSmartCtorD meta name fields =
  Exts.typeSigD (smartCtorId name) $
    Foldable.foldr' Exts.funT (Exts.conT (typeId name)) $
      map (fieldT meta Nothing) (filter fieldIsParam fields)

funSmartCtorD :: Id -> [Field] -> Exts.Decl
funSmartCtorD name fields =
  Exts.sfun (Exts.nameN (smartCtorId name)) puns (Exts.unguardedRhs rhs) Exts.noBinds
  where
    puns =
      map fieldPunE (filter fieldIsParam fields)

    rhs
      | null fields = Exts.varE (ctorId name)
      | otherwise = Exts.conR (ctorId name) (map update fields)

    update field
      | fieldMaybe field = Exts.fieldR label Exts.nothingE
      | fieldMonoid field = Exts.fieldR label Exts.memptyE
      | otherwise = Exts.punR label
      where
        label = fieldAccessor field

sigLensD :: HasMetadata meta Identity => meta -> Id -> Field -> Exts.Decl
sigLensD meta name field =
  Exts.typeSigD (fieldLens field) $
    Exts.appT (Exts.conT "Lens.Lens'") (Exts.conT (typeId name))
      `Exts.appT` fieldT meta (Just (typeId name)) field

funLensD :: HasMetadata meta Identity => meta -> Id -> Field -> Exts.Decl
funLensD meta name field =
  Exts.sfun
    (Exts.nameN (fieldLens field))
    []
    (Exts.unguardedRhs (fieldLensE field))
    Exts.noBinds

-- Service Configuration

sigServiceD :: HasMetadata meta Identity => meta -> Exts.Decl
sigServiceD meta =
  Exts.typeSigD (meta ^. serviceConfig) (Exts.conT "Core.Service")

funServiceD :: HasMetadata meta Identity => meta -> Retry -> Exts.Decl
funServiceD meta spec =
  Exts.patBindWhere (Exts.varP name) serviceRecord binds
  where
    serviceRecord =
      Exts.conR
        "Core.Service"
        [ Exts.fieldR "Core._svcAbbrev" (Exts.strE abbrev),
          Exts.fieldR "Core._svcSigner" (Exts.varE signer),
          Exts.fieldR "Core._svcPrefix" (Exts.strE prefix),
          Exts.fieldR "Core._svcVersion" (Exts.strE version),
          Exts.fieldR "Core._svcTimeout" (Exts.justE (Exts.intE 70)),
          Exts.fieldR "Core._svcCheck" (Exts.varE "Core.statusSuccess"),
          Exts.fieldR "Core._svcRetry" (Exts.varE "retry"),
          Exts.fieldR "Core._svcError" (Exts.app (Exts.varE error') (Exts.strE abbrev)),
          Exts.fieldR "Core._svcEndpoint" $
            Exts.app (Exts.varE "Core.defaultEndpoint") (Exts.varE name)
        ]

    binds =
      [ retry Exts.noBinds,
        check Exts.noBinds
      ]

    retry =
      Exts.sfun (Exts.nameN "retry") [] . Exts.unguardedRhs $
        Exts.conR
          ("Core." <> _delayType delay)
          [ Exts.fieldR "Core._retryBase" (Exts.fracE (_delayBase delay)),
            Exts.fieldR "Core._retryGrowth" (Exts.intE (_delayGrowth delay)),
            Exts.fieldR "Core._retryAttempts" (Exts.intE (_retryAttempts spec)),
            Exts.fieldR "Core._retryCheck" (Exts.varE "check")
          ]

    delay =
      _retryDelay spec

    check =
      Exts.sfun (Exts.nameN "check") [Exts.nameN "e"] (Exts.guardedRhs branches)
      where
        branches =
          mapMaybe policy (HashMap.toList (_retryPolicies spec))
            ++ [Exts.otherwiseRhs Exts.nothingE]

        policy :: (Text, Policy) -> Maybe Exts.GuardedRhs
        policy (k, v) =
          Exts.guardRhs
            <$> guard v
            <*> pure (Exts.justE (Exts.strE k))

        guard = \case
          When (WhenStatus (Just code) status) ->
            Just $
              Exts.appFun
                (Exts.varE "Lens.has")
                [ Exts.paren $
                    Exts.composeE
                      (Exts.app (Exts.varE "Core.hasCode") (Exts.strE code))
                      (Exts.app (Exts.varE "Core.hasStatus") (Exts.intE status)),
                  Exts.varE "e"
                ]
          --
          When (WhenStatus Nothing status) ->
            Just $
              Exts.appFun
                (Exts.varE "Lens.has")
                [ Exts.paren (Exts.app (Exts.varE "Core.hasStatus") (Exts.intE status)),
                  Exts.varE "e"
                ]
          --
          _other ->
            Nothing

    signer = "Sign." <> sigToText (meta ^. signatureVersion)
    error' = "Core." <> serviceError meta

    version = meta ^. apiVersion
    prefix = meta ^. endpointPrefix
    abbrev = meta ^. serviceAbbrev
    name = meta ^. serviceConfig

-- Errors

sigErrorD :: Text -> Exts.Decl
sigErrorD name =
  Exts.typeSigD name $
    Exts.forallT
      (Exts.assertT (Exts.conT "Core.AsError" `Exts.appT` Exts.varT "a"))
      $ Exts.conT "Lens.Getting"
        `Exts.appT` Exts.parenT (Exts.conT "Core.First" `Exts.appT` Exts.conT "Core.ServiceError")
        `Exts.appT` (Exts.varT "a")
        `Exts.appT` Exts.conT "Core.ServiceError"

funErrorD ::
  HasMetadata meta Identity =>
  meta ->
  Text ->
  Maybe Int ->
  Text ->
  Exts.Decl
funErrorD meta name mstatus code =
  Exts.sfun (Exts.nameN name) [] (Exts.unguardedRhs rhs) Exts.noBinds
  where
    rhs = maybe withCode withStatus mstatus

    withStatus n =
      Exts.composeE withCode $
        Exts.app (Exts.varE "Core.hasStatues") (Exts.intE (fromIntegral n))

    withCode =
      Exts.appFun
        (Exts.varE "Core._MatchServiceError")
        [ Exts.varE (meta ^. serviceConfig),
          Exts.strE code
        ]

-- Waiters

sigWaiterD :: Id -> Waiter a -> Exts.Decl
sigWaiterD name spec =
  Exts.typeSigD (smartCtorId name) $
    Exts.conT "Waiter.Wait"
      `Exts.appT` Exts.conT (typeId (_waitOperation spec))

funWaiterD :: Id -> Waiter Field -> Exts.Decl
funWaiterD name spec =
  Exts.sfun (Exts.nameN (smartCtorId name)) [] (Exts.unguardedRhs waitRecord) Exts.noBinds
  where
    waitRecord =
      Exts.conR
        "Waiter.Wait"
        [ Exts.fieldR "Waiter._waitName" (Exts.strE (memberId name)),
          Exts.fieldR "Waiter._waitAttempts" (Exts.intE (_waitAttempts spec)),
          Exts.fieldR "Waiter._waitDelay" (Exts.intE (_waitDelay spec)),
          Exts.fieldR "Waiter._waitAcceptors" $
            Exts.listE (map match (_waitAcceptors spec))
        ]

    match x =
      case (_acceptMatch x, _acceptArgument x) of
        (_, Just (App IsEmpty _)) ->
          Exts.appFun (Exts.varE "Waiter.matchEmpty") (expect x : criteria x : arguments x)
        (_, Just (App NotEmpty _)) ->
          Exts.appFun (Exts.varE "Waiter.matchNonEmpty") (expect x : criteria x : arguments x)
        (Path, _) ->
          Exts.appFun (Exts.varE "Waiter.matchAll") (expect x : criteria x : arguments x)
        (PathAll, _) ->
          Exts.appFun (Exts.varE "Waiter.matchAll") (expect x : criteria x : arguments x)
        (PathAny, _) ->
          Exts.appFun (Exts.varE "Waiter.matchAny") (expect x : criteria x : arguments x)
        (Status, _) ->
          Exts.appFun (Exts.varE "Waiter.matchStatus") (expect x : criteria x : arguments x)
        (Error, _) ->
          Exts.appFun (Exts.varE "Waiter.matchError") (expect x : criteria x : arguments x)

    expect x =
      case _acceptExpect x of
        Status' i -> Exts.intE i
        Boolean b -> Exts.conE ("Core." <> Text.pack (show b))
        Textual t -> Exts.strE t

    criteria x =
      Exts.varE $
        case _acceptCriteria x of
          Retry -> "Waiter.AcceptRetry"
          Success -> "Waiter.AcceptSuccess"
          Failure -> "Waiter.AcceptFailure"

    arguments x =
      maybeToList (notationE True <$> _acceptArgument x)

-- Instances

instancesD :: Protocol -> Id -> [Inst] -> [Exts.Decl]
instancesD protocol' name xs =
  mapMaybe declare xs
  where
    declare = \case
      FromJSON fields -> Just (classFromJSOND protocol' name fields)
      ToJSON fields -> Just (classToJSOND protocol' name fields)
      FromXML fields -> Just (classFromXMLD protocol' name fields)
      ToXML fields ->
        Just $
          classToXMLD protocol' name fields $
            listToMaybe [(mns, root) | ToElement mns root <- xs]
      -- Erasure
      ToElement {} -> Nothing
      ToHeaders {} -> Nothing
      ToPath {} -> Nothing
      ToQuery {} -> Nothing
      ToBody {} -> Nothing

classToJSOND :: Protocol -> Id -> [Field] -> Exts.Decl
classToJSOND protocol' name fields =
  Exts.instanceD
    "Core.FromJSON"
    (typeId name)
    [ Exts.funBindD
        [ Exts.wildM "toJSON" (typeId name) Nothing isEmpty Exts.noBinds $
            Exts.unguardedRhs $
              if isEmpty
                then Exts.app (Exts.varE "Core.Object") Exts.memptyE
                else toJSONE protocol' fields
        ]
    ]
  where
    isEmpty = null fields

classFromJSOND :: Protocol -> Id -> [Field] -> Exts.Decl
classFromJSOND protocol' name fields =
  Exts.instanceD
    "Core.FromJSON"
    (typeId name)
    [ Exts.funBindD
        [ Exts.nullM "parseJSON" Exts.noBinds $
            Exts.unguardedRhs $
              Exts.applyE
                (Exts.app (Exts.varE "Core.withObject") (Exts.strE (typeId name)))
                . Exts.lamE [Exts.varP "x"]
                $ Exts.applicativeE
                  (Exts.varE (ctorId name))
                  (map (parseJSONE protocol') fields)
        ]
    ]

classToXMLD ::
  Protocol ->
  Id ->
  [Field] ->
  Maybe (Maybe Text, Either Text Field) ->
  Exts.Decl
classToXMLD protocol' name fields document =
  Exts.instanceD
    "Core.ToXML"
    (typeId name)
    $ [ Exts.funBindD
          [ Exts.wildM "toXML" (typeId name) Nothing (null fields) Exts.noBinds $
              Exts.unguardedRhs (toXMLE protocol' fields)
          ]
      ]
      ++ case document of
        Nothing -> []
        Just (mns, root) ->
          [ Exts.funBindD
              [ Exts.nullM "toXMLDocument" Exts.noBinds $
                  Exts.unguardedRhs (toXMLDocumentE protocol' mns root)
              ]
          ]

classFromXMLD :: Protocol -> Id -> [Field] -> Exts.Decl
classFromXMLD protocol' name fields =
  Exts.instanceD
    "Core.FromXML"
    (typeId name)
    [ Exts.funBindD
        [ Exts.varsM "parseXML" ["x"] Exts.noBinds $
            Exts.unguardedRhs $
              Exts.applicativeE (Exts.varE (ctorId name)) $
                map (parseXMLE protocol') fields
        ]
    ]

classAWSPagerD :: Id -> Pager Field -> Exts.Decl
classAWSPagerD name pager =
  Exts.instanceD
    "Pager.AWSPager"
    (typeId name)
    [ Exts.funBindD
        [ Exts.varsM "page" ["rq", "rs"] Exts.noBinds rhs
        ]
    ]
  where
    rhs =
      case pager of
        Only t ->
          Exts.guardedRhs
            [ stop (notationE False (_tokenOutput t)),
              other [t]
            ]
        --
        Next ks t ->
          Exts.guardedRhs $
            [stop (notationE False (_tokenOutput t))]
              ++ map (stop . notationE True) (Foldable.toList ks)
              ++ [other [t]]
        --
        Many k (t :| ts) ->
          Exts.guardedRhs
            [ stop (notationE False k),
              check t ts,
              other (t : ts)
            ]

    stop expr =
      Exts.guardRhs
        (Exts.app (Exts.varE "Pager.stop") (viewResponse expr))
        Exts.nothingE

    check initial rest =
      Exts.guardRhs
        (Foldable.foldl' andAlso (isNothing initial) rest)
        Exts.nothingE
      where
        andAlso lhs token =
          Exts.infixApp lhs (Exts.opQ "Core.&&") (isNothing token)

        isNothing token =
          Exts.app (Exts.varE "Core.isNothing") $
            viewResponse (outputNotation token)

    other =
      Exts.otherwiseRhs
        . Exts.justE
        . Foldable.foldl' setRequest (Exts.varE "rq")
      where
        setRequest lhs token =
          Exts.infixApp lhs (Exts.opQ "Core.&") $
            Exts.infixApp
              (inputNotation token)
              (Exts.opQ "Lens..~")
              (viewResponse (outputNotation token))

    inputNotation token =
      notationE False (_tokenInput token)

    outputNotation token =
      notationE False (_tokenOutput token)

    viewResponse expr =
      Exts.infixApp
        (Exts.varE "rs")
        (Exts.opQ (Exts.fieldGetterN expr))
        expr

classAWSRequestD ::
  HasMetadata meta Identity =>
  Config ->
  meta ->
  HTTP ->
  (Ref, [Inst], [Field]) ->
  (Ref, [Field]) ->
  Exts.Decl
classAWSRequestD cfg meta http (rqRef, rqInsts, rqFields) (rsRef, rsFields) =
  Exts.instanceD
    "Core.AWSRequest"
    rqName
    [ Exts.associatedTypeD "Rs" rqName rsName,
      funRequestD cfg meta http rqRef rqInsts rqFields,
      funResponseD (meta ^. protocol) rsRef rsFields
    ]
  where
    rqName =
      typeId (identifier rqRef)

    rsName =
      if isShared (Comonad.extract (_refAnn rsRef))
        then "Types." <> typeId (identifier rsRef)
        else typeId (identifier rsRef)

funResponseD :: Protocol -> Ref -> [Field] -> Exts.InstDecl
funResponseD protocol' ref fields =
  Exts.funBindD
    [ Exts.nullM "response" Exts.noBinds (Exts.unguardedRhs rhs)
    ]
  where
    rhs = Exts.app responseFunction rhsBody

    responseFunction
      | null fields =
        Exts.varE "Response.receiveNull"
      | isStreaming =
        Exts.varE "Response.receiveBody"
      | isLiteralPayload =
        Exts.varE "Response.receiveBytes"
      | Just x <- _refResultWrapper ref =
        Exts.app (Exts.varE (suffix <> "Wrapper")) (Exts.strE x)
      | isNotBody =
        Exts.varE "Response.receiveEmpty"
      | otherwise = Exts.varE suffix
      where
        suffix =
          "Response.receive" <> Protocol.suffix protocol'

    rhsBody
      | null fields = ctor
      | isSharedBody = rhsLambda parseAll
      | otherwise = rhsLambda $ Exts.applicativeE ctor $ map parseField fields

    rhsLambda =
      Exts.lamE [Exts.varP "s", Exts.varP "h", Exts.varP "x"]

    parseField x =
      case fieldLocation x of
        Just Headers -> parseHeadersE protocol' x
        Just Header -> parseHeadersE protocol' x
        Just StatusCode -> parseStatusE x
        Just Body | isStreaming -> Exts.pureE bind
        Nothing | isStreaming -> Exts.pureE bind
        _ -> parseProtocol x

    parseProtocol x =
      case protocol' of
        _ | _fieldPayload x -> parseOne x
        Json -> parseJSONE protocol' x
        RestJson -> parseJSONE protocol' x
        ApiGateway -> parseJSONE protocol' x
        _ -> parseXMLE protocol' x

    parseOne x
      | fieldLit x =
        if fieldMaybe x
          then Exts.pureE bind
          else Exts.pureE (Exts.justE bind)
      -- This ensures anything which is set as a payload,
      -- but is a primitive type, is just consumed as a bytestring.
      | otherwise = parseAll

    parseAll
      | isLiteralPayload = Exts.pureE bind
      | otherwise =
        flip Exts.app bind . Exts.varE $
          case protocol' of
            Json -> "Core.eitherParseJSON"
            RestJson -> "Core.eitherParseJSON"
            ApiGateway -> "Core.eitherParseJSON"
            _ -> "Core.parseXML"

    bind = Exts.varE "x"
    ctor = Exts.varE (ctorId name)
    name = identifier ref

    isStreaming =
      any fieldStream fields

    isNotBody =
      all (not . fieldBody) fields

    isSharedBody =
      all fieldBody fields
        && isShared (Comonad.extract (_refAnn ref))

    isLiteralPayload =
      any fieldLitPayload fields

funRequestD ::
  HasMetadata meta Identity =>
  Config ->
  meta ->
  HTTP ->
  Ref ->
  [Inst] ->
  [Field] ->
  Exts.InstDecl
funRequestD cfg meta http ref instances fields =
  Exts.funBindD
    [ Exts.wildM "request" "Core.Request" (Just "x") (null fields) Exts.noBinds $
        Exts.unguardedRhs extendedRhs
    ]
  where
    extendedRhs =
      maybe newRequest (Foldable.foldr' applyPlugin newRequest) validPlugins

    validPlugins =
      HashMap.lookup (identifier ref) (_operationPlugins cfg)

    applyPlugin name =
      Exts.applyE (Exts.varE ("Request." <> name))

    newRequest =
      case listToMaybe (mapMaybe bodyFunction instances) of
        Nothing -> requestRecord queryVar (headersVar []) (Exts.strE "")
        Just mk -> mk

    bodyFunction = \case
      ToBody field ->
        Just $
          requestRecord
            queryVar
            (headersVar [])
            (Exts.app (Exts.varE "Core.toBody") (Exts.varE (fieldAccessor field)))
      --
      ToJSON {} ->
        Just $
          requestRecord
            queryVar
            (headersVar [])
            (Exts.app (Exts.varE "Core.toJSONBody") (Exts.varE "x"))
      --
      ToElement {} ->
        Just $
          requestRecord
            queryVar
            (headersVar [])
            (Exts.app (Exts.varE "Core.toXMLBody") (Exts.varE "x"))
      --
      _other
        | method' == POST && (protocol' == Query || protocol' == Ec2) ->
          Just $
            requestRecord
              Exts.memptyE
              (headersVar [Left ("Content-Type", hFormEncoded)])
              (Exts.app (Exts.varE "Core.toFormBody") queryVar)
      --
      _other ->
        Nothing

    hFormEncoded =
      "application/x-www-form-urlencoded; charset=utf-8"

    requestRecord query headers body =
      Exts.conR
        "Core.Request"
        [ Exts.fieldR "Core._rqService" (Exts.varE ("Types." <> service')),
          Exts.fieldR "Core._rqMethod" (Exts.varE ("Request." <> methodToText method')),
          Exts.fieldR "Core._rqPath" (Exts.app (Exts.varE "Core.rawPath") pathVar),
          Exts.fieldR "Core._rqQuery" query,
          Exts.fieldR "Core._rqHeaders" headers,
          Exts.fieldR "Core._rqBody" body
        ]

    pathVar =
      newVar [toPathE xs | ToPath xs <- instances]

    headersVar extra =
      newVar [toHeadersE protocol' (extra ++ xs) | ToHeaders xs <- instances]

    queryVar =
      newVar [toQueryE protocol' xs | ToQuery xs <- instances]

    newVar =
      fromMaybe Exts.memptyE
        . listToMaybe

    service' = meta ^. serviceConfig
    protocol' = meta ^. protocol
    method' = http ^. method

-- Expression Decoders

parseStatusE :: Field -> Exts.Exp
parseStatusE field =
  Exts.pureE $
    (if fieldMaybe field then Exts.justE else id) $
      Exts.app (Exts.varE "Core.fromEnum") (Exts.varE "s")

parseHeadersE :: Protocol -> Field -> Exts.Exp
parseHeadersE protocol' field
  | TMap {} <- typeOf field =
    Exts.appFun (Exts.varE "Core.parseHeaderMap") [name, bind]
  --
  | fieldMaybe field =
    Exts.appFun (Exts.varE "Core.parseHeaderMaybe") [name, bind]
  --
  | otherwise =
    Exts.appFun (Exts.varE "Core.parseHeader") [name, bind]
  where
    name = Exts.strE (memberName protocol' Output field)
    bind = Exts.varE "h"

parseJSONE :: Protocol -> Field -> Exts.Exp
parseJSONE protocol' field
  | fieldMaybe field =
    Exts.infixApp bind (Exts.opQ "Core..:?") name
  --
  | fieldMonoid field && not (fieldMaybe field) =
    Exts.infixApp
      (Exts.infixApp bind (Exts.opQ "Core..:?") name)
      (Exts.opQ "Core..!=")
      Exts.memptyE
  --
  | otherwise =
    Exts.infixApp bind (Exts.opQ "Core..:") name
  where
    name = Exts.strE (memberName protocol' Output field)
    bind = Exts.varE "x"

parseXMLE :: Protocol -> Field -> Exts.Exp
parseXMLE protocol' field =
  case nestedNames protocol' Output field of
    NMap mname itemPrefix keyPrefix valPrefix ->
      unflatten mname $
        Exts.appFun
          (Exts.varE "Core.parseXMLMap")
          [Exts.strE itemPrefix, Exts.strE keyPrefix, Exts.strE valPrefix]
    --
    NList mname itemPrefix
      | fieldMonoid field ->
        unflatten mname $
          Exts.appFun
            (Exts.varE "Core.parseXMLList")
            [Exts.strE itemPrefix]
      --
      | otherwise ->
        unflatten mname $
          Exts.appFun
            (Exts.varE "Core.parseXMLNonEmpty")
            [Exts.strE itemPrefix]
    --
    NName {} ->
      parseField
  where
    unflatten mname rhs =
      case mname of
        Nothing -> parseField
        Just {} -> Exts.infixApp parseField (Exts.opQ "Core..<@>") rhs

    parseField
      | fieldMaybe field =
        Exts.infixApp bind (Exts.opQ "Core..@?") name
      --
      | fieldMonoid field && not (fieldMaybe field) =
        Exts.infixApp
          (Exts.infixApp bind (Exts.opQ "Core..@?") name)
          (Exts.opQ "Core..@!")
          Exts.memptyE
      --
      | otherwise =
        Exts.infixApp bind (Exts.opQ "Core..@") name

    name = Exts.strE (memberName protocol' Output field)
    bind = Exts.varE "x"

-- Expression Encoders

toPathE :: [Either Text Field] -> Exts.Exp
toPathE = \case
  [] -> Exts.strE "/"
  [Left text] -> Exts.strE text
  x : xs -> Foldable.foldl' toPath (toText x) xs
    where
      toPath e a =
        Exts.mappendE e (Exts.paren (toText a))

      toText =
        either Exts.strE (Exts.app (Exts.varE "Core.toText") . fieldSelectE)

toQueryE :: Protocol -> [Either (Text, Maybe Text) Field] -> Exts.Exp
toQueryE protocol' = \case
  [] -> Exts.memptyE
  x : xs -> Foldable.foldl' toQuery (toValue x) xs
  where
    toQuery e a =
      Exts.mappendE e (Exts.paren (toValue a))

    toValue = \case
      Left (k, v) ->
        Exts.pureE (Exts.tuple [Exts.strE k, Exts.strE (fromMaybe "" v)])
      Right field ->
        flatFieldEncoderE
          protocol'
          (Exts.varE "Core.toQueryValue")
          (Exts.varE "Core.toQueryMap")
          (Exts.varE "Core.toQueryList")
          field

toHeadersE :: Protocol -> [Either (Text, Text) Field] -> Exts.Exp
toHeadersE protocol' = \case
  [] -> Exts.memptyE
  x : xs -> Foldable.foldl' toHeaders (toValue x) xs
  where
    toHeaders e a =
      Exts.mappendE e (Exts.paren (toValue a))

    toValue = \case
      Left (k, v) -> Exts.pureE (Exts.tuple [Exts.strE k, Exts.strE v])
      Right field ->
        fieldEncoderE
          protocol'
          (Exts.varE "Core.toHeaders")
          field

toXMLDocumentE :: Protocol -> Maybe Text -> Either Text Field -> Exts.Exp
toXMLDocumentE protocol' mnamespace = \case
  Left name -> createRoot name []
  Right field -> createNode field
  where
    createRoot name =
      Exts.appFun (Exts.varE "Core.mkXMLElement")
        . (Exts.strE (qualifyName name) :)

    createNode field =
      createRoot
        (memberName protocol' Input field)
        [ Exts.varE "Core..",
          fieldSelectE field
        ]

    qualifyName name
      | Just ns <- mnamespace = "{" <> ns <> "}" <> name
      | otherwise = name

toXMLE :: Protocol -> [Field] -> Exts.Exp
toXMLE protocol' = \case
  [] -> Exts.varE "Core.mempty"
  x : xs -> Foldable.foldl' toXML (toValue x) xs
  where
    toXML e a =
      Exts.mappendE e (toValue a)

    toValue field =
      flatFieldEncoderE
        protocol'
        (toNode field)
        (Exts.varE "Core.toXMLMap")
        (Exts.varE "Core.toXMLList")
        field

    toNode field =
      Exts.varE $
        if field ^. fieldRef . refXMLAttribute
          then "Core.toXMLAttribute"
          else "Core.toXMLNode"

toJSONE :: Protocol -> [Field] -> Exts.Exp
toJSONE protocol' =
  Exts.app (Exts.varE "Core.object")
    . Exts.app (Exts.varE "Core.catMaybes")
    . Exts.listE
    . map (infixFieldEncoderE protocol' "Core..=")

-- ToJSON
infixFieldEncoderE :: Protocol -> Text -> Field -> Exts.Exp
infixFieldEncoderE protocol' toItem field =
  let name = Exts.strE (memberName protocol' Input field)
      selector = fieldSelectE field
   in if fieldMaybe field
        then Exts.paren (Exts.app name (Exts.varE toItem)) `Exts.fmapE` selector
        else Exts.justE (Exts.infixApp name (Exts.opQ toItem) selector)

-- ToHeaders
fieldEncoderE :: Protocol -> Exts.Exp -> Field -> Exts.Exp
fieldEncoderE protocol' toItem field =
  Exts.appFun
    toItem
    [ Exts.strE (memberName protocol' Input field),
      fieldSelectE field
    ]

-- ToXML, ToForm/ToQuery
flatFieldEncoderE ::
  Protocol ->
  Exts.Exp ->
  Exts.Exp ->
  Exts.Exp ->
  Field ->
  Exts.Exp
flatFieldEncoderE protocol' toItem toMap toList field =
  case nestedNames protocol' Input field of
    NMap mname itemPrefix keyPrefix valPrefix
      | isMaybe ->
        flatten mname $
          Exts.appFun
            toMap
            [Exts.strE itemPrefix, Exts.strE keyPrefix, Exts.strE valPrefix]
            `Exts.fmapE` selector
      --
      | otherwise ->
        flatten mname $
          Exts.appFun
            toMap
            [Exts.strE itemPrefix, Exts.strE keyPrefix, Exts.strE valPrefix, selector]
    --
    NList mname itemPrefix
      | isMaybe ->
        flatten mname $
          Exts.app toList (Exts.strE itemPrefix)
            `Exts.fmapE` selector
      --
      | otherwise ->
        flatten mname $
          Exts.appFun toList [Exts.strE itemPrefix, selector]
    --
    NName name
      | isMaybe ->
        Exts.app toItem (Exts.strE name)
          `Exts.fmapE` selector
      --
      | otherwise ->
        Exts.appFun toItem [Exts.strE name, selector]
  where
    isMaybe =
      fieldMaybe field

    selector =
      fieldSelectE field

    flatten = \case
      Just name -> Exts.app (Exts.app toItem (Exts.strE name))
      Nothing -> id

-- FIXME: doesn't support Maybe fields correctly.
notationE :: Bool -> Notation Field -> Exts.Exp
notationE nested = \case
  Key x -> field x
  App f a -> apply (loop a) f
  Dot a b -> Exts.composeE (loop a) (loop b)
  Alt a b -> Exts.altE (loop a) (loop b)
  where
    loop = notationE nested

    -- branch e =
    --   Exts.paren (Exts.app (Exts.varE (Exts.fieldGetterN e)) e)

    apply e = \case
      IsEmpty ->
        Exts.composeE e (Exts.varE "Core._isEmpty")
      --
      NotEmpty ->
        Exts.composeE e (Exts.varE "Core._isNonEmpty")
      --
      Each ->
        Exts.app (Exts.varE "Lens.folding")
          . Exts.paren
          . Exts.app (Exts.varE "Lens.concatOf")
          . Exts.composeE e
          $ Exts.app (Exts.varE "Lens.to") (Exts.varE "Core.toList")
      --
      Last ->
        Exts.composeE e (Exts.varE "Lens._last")

    field x
      | not nested = lens
      | fieldMaybe x = Exts.composeE lens (Exts.varE "Lens._Just")
      | otherwise = lens
      where
        lens = fieldLensE x

-- "Certificate.DomainValidationOptions[].ValidationStatus.Foo"

-- (Dot (Key Certificate<Certificate>)
--   (Dot (App Each (Key DomainValidationOptions<DomainValidationOptions>))
--     (Dot (Key ValidationStatus<ValidationStatus>) (Key Foo<Foo>))))

-- labels (k :| ks) =
--   if null ks
--     then label nested k
--     else Foldable.foldl' compose (label nested k) ks
--   where
--     compose e x =
--       Exts.composeE e (label True x)

-- label nest = \case
--   Key field -> key nest field
--   Last field -> Exts.composeE (key nest field) (Exts.varE "Lens._last")
--   Each field ->
--     Exts.app (Exts.varE "Lens.folding")
--       . Exts.paren
--       . Exts.app (Exts.varE "Lens.concatOf")
--       . Exts.composeE (key nest field)
--       $ Exts.app (Exts.varE "Lens.to") (Exts.varE "Core.toList")

-- key nest field
--   | not nest = lens
--   | fieldMaybe field = Exts.composeE lens (Exts.varE "Lens._Just")
--   | otherwise = lens
--   where
--     lens = fieldLensE field

-- Fields

fieldLensE :: Field -> Exts.Exp
fieldLensE = Exts.fieldLensE . fieldAccessor

fieldSelectE :: Field -> Exts.Exp
fieldSelectE = Exts.varE . fieldAccessor

fieldPunE :: Field -> Exts.Name
fieldPunE = Exts.nameN . fieldAccessor

memberName :: Protocol -> Direction -> Field -> Text
memberName protocol direction field =
  Protocol.memberName protocol direction (_fieldId field) (_fieldRef field)

nestedNames :: Protocol -> Direction -> Field -> Names
nestedNames protocol direction field =
  Protocol.nestedNames protocol direction (_fieldId field) (_fieldRef field)

-- Types

fieldT ::
  HasMetadata meta Identity =>
  meta ->
  Maybe Text ->
  Field ->
  Exts.Type
fieldT meta ignorePrefix field =
  typeT meta ignorePrefix (_fieldDirection field) (typeOf field)

typeT ::
  HasMetadata meta Identity =>
  meta ->
  Maybe Text ->
  Maybe Direction ->
  TType ->
  Exts.Type
typeT meta ignorePrefix direction = loop
  where
    loop = \case
      TType x _
        | Just x == ignorePrefix -> Exts.conT x
        | otherwise -> Exts.conT ("Types." <> x)
      --
      TLit x ->
        litT (runIdentity (meta ^. timestampFormat)) x
      --
      TNatural ->
        Exts.conT "Core.Natural"
      --
      TSensitive x ->
        Exts.appT (Exts.conT "Core.Sensitive") (loop x)
      --
      TMaybe x ->
        Exts.appT (Exts.conT "Core.Maybe") (loop x)
      --
      TList x ->
        Exts.listT (loop x)
      --
      TList1 x ->
        Exts.appT (Exts.conT "Core.NonEmpty") (loop x)
      --
      TMap k v ->
        Exts.appT
          (Exts.appT (Exts.conT "Core.HashMap") (loop k))
          (Exts.parenT (loop v))
      --
      TStream ->
        streamingT (meta ^. signatureVersion) direction

streamingT version =
  Exts.conT . \case
    Nothing -> "Core.RsBody"
    Just Output -> "Core.RsBody" -- Response stream.
    Just Input
      -- If the signer supports chunked encoding, both body types are accepted.
      | version == S3 -> "Core.RqBody"
      -- Otherwise only a pre-hashed body is accepted.
      | otherwise -> "Core.HashedBody"

litT :: Timestamp -> Lit -> Exts.Type
litT ts =
  Exts.conT . \case
    Bool -> "Core.Bool"
    Int -> "Core.Int"
    Long -> "Core.Integer"
    Double -> "Core.Double"
    Text -> "Core.Text"
    Bytes -> "Core.ByteString"
    Base64 -> "Core.Base64"
    Time mts -> "Core." <> tsToText (fromMaybe ts mts)
    JsonValue -> "Core.ByteString"
