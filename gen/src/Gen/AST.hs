{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Gen.AST
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.AST (transformAST) where

import           Control.Applicative        (pure, (<$>), (<*>), (<|>))
import           Control.Arrow              ((&&&))
import           Control.Error
import           Control.Lens               hiding (Indexed, filtered, ignored,
                                             indexed, op)
import           Control.Monad
import           Control.Monad.State.Strict
import qualified Data.CaseInsensitive       as CI
import           Data.Char
import           Data.Foldable              (foldMap)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as Set
import           Data.List                  (find, group, nub, sort)
import           Data.Monoid                hiding (Product)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Text.Manipulate
import           Gen.Input                  hiding (Operation (..))
import qualified Gen.Input                  as Input
import           Gen.Names
import           Gen.Output
import           Gen.Types

transformAST :: Retries -> Model -> Input -> Output
transformAST Retries{..} m inp = Output cabal service ops types waiters
  where
    cabal = Cabal
        { _cName         = name
        , _cUrl          = url
        , _cAbbrev       = abbrev
        , _cLibrary      = overrides ^. oLibrary
        , _cVersion      = overrides ^. oVersion
        , _cDescription  = description (inp ^. inpDocumentation)
        , _cProtocol     = protocol
        , _cExposed      = external
        , _cOther        = internal
        }

    service = Service
        { _svName           = name
        , _svUrl            = url
        , _svAbbrev         = abbrev
        , _svNamespace      = namespace [unAbbrev abbrev]
        , _svImports        = imports
        , _svVersion        = version
        , _svDocumentation  = above (inp ^. inpDocumentation)
        , _svProtocol       = protocol
        , _svEndpointPrefix = endpointPrefix
        , _svSignature      = inp ^. mSignatureVersion
        , _svChecksum       = checksum
        , _svXmlNamespace   = xmlNamespace
        , _svTargetPrefix   = inp ^. mTargetPrefix
        , _svJsonVersion    = inp ^. mJsonVersion
        , _svError          = errorType protocol abbrev
        , _svRetryDelay     = delay
        , _svRetryPolicies  = policies
        }

    types = Types
        { _tNamespace = typesNamespace
        , _tImports   = overrides ^. oTypesModules
        , _tTypes     = filter (not . isVoid) (Map.elems ts)
        , _tShared    = share
        }

    waiters = Waiters
        { _wNamespace = waitersNamespace
        , _wImports   = imps
        , _wWaiters   = ws
        }
      where
        ws = prefixWaiters (ts <> os)
           . Map.filterWithKey f
           $ inp ^. inpWaiters

        os = foldMap operationDataTypes ops

        f k _ = not $ Set.member (CI.mk k) (overrides ^. oIgnoreWaiters)

        imps = sort
            . nub
            $ typesNamespace
            : map (operationNS abbrev . _wOperation) (Map.elems ws)

    anyWaiters = not . Map.null $ _wWaiters waiters

    imports = sort $
          typesNamespace
        : operationNamespaces
       ++ [waitersNamespace | anyWaiters]

    external = sort $
          service ^. svNamespace
        : typesNamespace
        : operationNamespaces
       ++ [waitersNamespace | anyWaiters]

    internal = sort $
          (overrides ^. oOperationsModules)
       ++ (overrides ^. oTypesModules)

    typesNamespace      = typesNS abbrev
    waitersNamespace    = waitersNS abbrev
    operationNamespaces = sort (map (view opNamespace) ops)

    name = "Amazon " <> stripAWS (inp ^. mServiceFullName)

    url = overrides ^. oUrl

    abbrev   = inp ^. mServiceAbbreviation
    protocol = inp ^. mProtocol
    version  = inp ^. mApiVersion

    (ops, ts, share) = dataTypes overrides abbrev inp

    endpointPrefix = inp ^. mEndpointPrefix

    overrides = m ^. mOverrides

    checksum = fromMaybe SHA256 (inp ^. mChecksumFormat)

    xmlNamespace
        | Just x <- inp ^. mXmlNamespace         = Just x
        | protocol `elem` [Query, Xml, RestXml] = Just $
               "http://"
            <> endpointPrefix
            <> ".amazonaws.com/doc/"
            <> version
            <> "/"
        | otherwise                             = Nothing

    delay = Exp
        { _eAttempts = retry ^. rMaxAttempts
        , _eBase     = retry ^. rDelay . dBase
        , _eGrowth   = retry ^. rDelay . dGrowthFactor
        }

    policies = Map.fromList . mapMaybe go . Map.toList $ retry ^. rPolicies
      where
        go :: (a, Policy) -> Maybe (a, RetryPolicy)
        go (k, p)
            | ApplyWhen (WhenStatus e c) <- p          = Just (k, Status e c)
            | ApplyRef r <- p
            , Just x     <- Map.lookup r _rDefinitions = go (k, x)
            | otherwise                                = Nothing

    retry = fromMaybe _rDefault $
            Map.lookup endpointPrefix _rRetries
        <|> Map.lookup (Text.pack (m ^. mName)) _rRetries

dataTypes :: Overrides
          -> Abbrev
          -> Input
          -> ([Operation], HashMap Text Data, HashSet Text)
dataTypes o a inp = res (runState run ds)
  where
    res (x, y) = (sort (Map.elems x), y, ss)

    run = Map.traverseWithKey
        (operation a proto url (o ^. oOperationsModules) ss (inp ^. inpPagination))
        (inp' ^. inpOperations)

    (inp', prefixed -> ds) = runState (requests inp ss) datas

    ss = evalState share datas

    share = shared inp

    datas = overriden overrides $
        shapes proto (defaultTS proto (inp ^. mTimestampFormat)) (inp ^. inpShapes)

    proto     = inp ^. mProtocol
    url       = o ^. oOperationUrl
    overrides = o ^. oOverrides

-- | Insert a new request datatype for any shared input, and update
-- the operations accordingly.
requests :: Input -> HashSet Text -> State (HashMap Text Data) Input
requests inp ss = do
    os <- Map.traverseWithKey go (inp ^. inpOperations)
    return $! inp & inpOperations .~ os
  where
    go :: Text -> Input.Operation -> State (HashMap Text Data) Input.Operation
    go n o = do
        rq <- update n (o ^. oInput)
        rs <- update (n <> "Response") (o ^. oOutput)
        return $! o
            & oInput  .~ rq
            & oOutput .~ rs

    update :: Text -> Maybe Ref -> State (HashMap Text Data) (Maybe Ref)
    update _ Nothing            = return Nothing
    update n (Just r) = do
        let k = r ^. refShape
        md <- gets (Map.lookup k)
        case md of
            Just d | Set.member k ss -> copy n r d
            Just d                   -> move n k r d
            Nothing                  -> return Nothing

    move :: Text
         -> Text
         -> Ref
         -> Data
         -> State (HashMap Text Data) (Maybe Ref)
    move n k r d = modify (Map.delete k) >> copy n r d

    copy :: Text
         -> Ref
         -> Data
         -> State (HashMap Text Data) (Maybe Ref)
    copy k r d = do
        modify (Map.insert k (dataRename k d))
        return (Just (r & refShape .~ k))

operation :: Abbrev
          -> Protocol
          -> Text
          -> [NS]
          -> HashSet Text
          -> HashMap Text (Pager ())
          -> Text
          -> Input.Operation
          -> State (HashMap Text Data) Operation
operation a proto base ns ss pgs n o = do
    inp <- request  (o ^. oInput)
    out <- response (o ^. oOutput)
    op inp out <$> pager inp out (Map.lookup n pgs)
  where
    op rq rs pg = Operation
        { _opName             = n
        , _opUrl              = base <> n <> ".html"
        , _opService          = a
        , _opProtocol         = proto
        , _opNamespace        = operationNS a n
        , _opImports          = requestNS a proto : typesNS a : ns
        , _opDocumentation    = above <$> o ^. oDocumentation
        , _opDocumentationUrl = o ^. oDocumentationUrl
        , _opMethod           = o ^. oHttp.hMethod
        , _opRequest          = rq
        , _opResponse         = rs
        , _opPager            = pg
        }

    request = go (\x k s d -> Request proto (prefixURI x d) k s d) True

    prefixURI k d = o ^. oHttp . hRequestUri & uriSegments %~ f
      where
        f (Seg x)                     = Seg x
        f (Var x)
            | Just y <- x `lookup` ls = Var y
            | otherwise               = Var (k <> x)

        ls = fieldLocations d

    response r = go (const (Response proto w k)) False r
      where
        w = fromMaybe False (join (_refWrapper <$> r))
        k = join (_refResultWrapper <$> r)

    rqName = n
    rsName = n <> "Response"

    go :: (Text -> Text -> Bool -> Data -> a)
       -> Bool
       -> Maybe Ref
       -> State (HashMap Text Data) a
    go c rq Nothing  = return $! placeholder c rq
    go c rq (Just x) = do
        let k = x ^. refShape
        m <- gets (^. at k)
        case m of
            Nothing   -> return $! placeholder c rq
            Just Void -> return $! placeholder c rq
            Just d    -> do
                let d' = setStreaming rq d
                    t  = fromMaybe "" (fieldPrefix d')
                    p  = Set.member k ss
                unless p $
                    modify (Map.delete k)
                return $! c t k p d'

    placeholder c True  = c "" rqName False (Empty rqName)
    placeholder c False = c "" rsName False (Empty rsName)

-- | Prefix the nested field access and index notation of a pager by introspecting
-- the state fields.
pager :: Request
      -> Response
      -> Maybe (Pager ())
      -> State (HashMap Text Data) (Maybe (Pager Type))
pager _   _   Nothing   = return Nothing
pager inp out (Just pg) = get >>= go
  where
    go ds = return . Just $!
        case pg of
            More m t -> More (label rs m) (map token t)
            Next r t -> Next (label rs r) (token t)
      where
        ts = Map.fromList [(rq, _rqData inp), (rs, _rsData out)] <> ds

        token (Token _ _ i o) =
            let x = label rq i
                y = label rs o
             in Token (require (_rqData inp) x) (require (_rsData out) y) x y

        rq = _rqName inp
        rs = _rsName out

        require d k
            | Just n <- key k
            , Just f <- find ((n ==) . _fName) (toListOf dataFields d)
                = _fType f
            | otherwise
                = TType "Unknown"

        key (Key    n)   = Just n
        key (Index  n _) = Just n
        key (Apply  n _) = Just n
        key _            = Nothing

        label _ NoKey        = NoKey
        label x (Key    n)   = Key    (applied x n)
        label x (Index  n k) = Index  (applied x n) (label (indexed x n) k)
        label x (Apply  n k) = Apply  (applied x n) (label n k)
        label x (Choice n k) = Choice (label x n)   (label x k)

        applied x n
            | Just d <- Map.lookup x ts
            , Just f <- findField n d = _fName f
            | otherwise               = error $
                "Unable to apply field "
                    ++ show n
                    ++ " in datatype "
                    ++ show x
                    ++ "\n"
                    ++ show (Map.keys ts)

        indexed x n
            | Just d         <- Map.lookup x ts
            , Just f         <- findField n d
            , Just (TType l) <- listElement (f ^. typeOf) = l
            | otherwise           = error $
                "Unable to index field "
                    ++ show n
                    ++ " in datatype "
                    ++ show x
                    ++ "\n"
                    ++ show (Map.keys ts)

-- | Find any datatypes that are shared as operation inputs/outputs.
shared :: Input -> State (HashMap Text Data) (HashSet Text)
shared inp = do
    xs <- forM ops $ \o ->
        (++) <$> ins (o ^. oInput)
             <*> ins (o ^. oOutput)
    return $! occur (freq (concat xs))
  where
    ops = Map.elems (inp ^. inpOperations)

    occur = Set.fromList . mapMaybe snd . filter ((> 1) . fst)
    freq  = map (length &&& headMay) . group . sort

    ins :: Maybe Ref -> State (HashMap Text Data) [Text]
    ins Nothing  = return []
    ins (Just r) = do
        let k = r ^. refShape
        md <- gets (Map.lookup k)
        return $! maybe [] (nested k) md

    nested :: Text -> Data -> [Text]
    nested _ Nullary{} = []
    nested k d         = k : vs
      where
        vs = mapMaybe name
           . concatMap (universeOn typeOf)
           $ toListOf dataFields d

    name :: Type -> Maybe Text
    name (TType k) = Just k
    name _         = Nothing

prefixed :: HashMap Text Data -> HashMap Text Data
prefixed m = Map.fromList $ evalState (mapM run (Map.toList m)) mempty
  where
    run (k, x) = (k,) <$> go k (prefix k) x

    prefix k = Text.toLower (fromMaybe def (toAcronym (suffix k)))
      where
        def | Text.length k <= 3 = k
            | otherwise          = Text.take 1 k

    suffix k = fromMaybe k ("Request" `Text.stripSuffix` k)

    names = Set.fromList (Map.keys m)

    go :: Text -> Text -> Data -> State (HashSet Text) Data
    go n k v1
        | Nullary{} <- v1 = do
            let v2 = mapFieldNames (enumName n False "") v1
                v3 = mapFieldNames (enumName n True  k)  v2
                v4 = mapFieldNames (enumName n False n)  v1

                f1 = Set.fromList (fieldNames v2)
                f2 = Set.fromList (fieldNames v3)
                f3 = Set.fromList (fieldNames v4)

            s <- get

            let p1 = Set.null (Set.intersection f1 s)
                p2 = Set.null (Set.intersection f2 s)
                p3 = Set.null (Set.intersection f3 s)

                d1 = Set.null (Set.intersection f1 names)
                d2 = Set.null (Set.intersection f2 names)
                d3 = Set.null (Set.intersection f3 names)

            if | p1, d1    -> modify (mappend f1) >> return v2
               | p2, d2    -> modify (mappend f2) >> return v3
               | p3, d3    -> modify (mappend f3) >> return v4
               | otherwise ->
                   error $ "Unabled to generate enum fields for: " ++ show n

        | otherwise       = do
            let v2 = mapFieldNames (mappend k . upperHead) v1
                fs = Set.fromList (fieldNames v2)

            p <- gets (Set.null . Set.intersection fs)

            if | p         -> modify (mappend fs) >> return v2
               | otherwise -> go n (numericSuffix k) v1

overriden :: HashMap Text Override -> HashMap Text Data -> HashMap Text Data
overriden = flip (Map.foldlWithKey' run)
  where
    run :: HashMap Text Data -- ^ acc
        -> Text              -- ^ key
        -> Override          -- ^ val
        -> HashMap Text Data
    run r k o =
          renameTo   k (o ^. oRenameTo)
        . replacedBy k (o ^. oReplacedBy)
        . sumPrefix  k (o ^. oSumPrefix)
        . sumValues  k (o ^. oSumValues)
        . Map.adjust (dataFields %~ fld) k
        $ r
      where
        fld = required (o ^. oRequired) (o ^. oOptional)
            . renamed  (o ^. oRenamed)

    -- Types:

    renameTo :: Text -> Maybe Text -> HashMap Text Data -> HashMap Text Data
    renameTo _ Nothing  m = m
    renameTo x (Just y) m = replaced x y $
        case Map.lookup x m of
            Nothing -> m
            Just z  -> Map.insert y (ren z) (Map.delete x m)
      where
        ren = \case
            Newtype _ f  -> Newtype y f
            Record  _ fs -> Record  y fs
            Product _ fs -> Product y fs
            Nullary _ m' -> Nullary y m'
            Empty   _    -> Empty   y
            Void         -> Void

    replacedBy :: Text -> Maybe Text -> HashMap Text Data -> HashMap Text Data
    replacedBy _ Nothing  = id
    replacedBy x (Just y) = Map.filterWithKey (const . (/= x)) . replaced x y

    replaced :: Text -> Text -> HashMap Text Data -> HashMap Text Data
    replaced x y = Map.map (dataFields . typeOf %~ retype)
      where
        retype :: Type -> Type
        retype = \case
            TPrim      z    -> TPrim      z
            TMaybe     z    -> TMaybe     (retype z)
            TFlatten   z    -> TFlatten   (retype z)
            TSensitive z    -> TSensitive (retype z)
            TCase      z    -> TCase      (retype z)
            TList  e   z    -> TList    e (retype z)
            TList1 e   z    -> TList1   e (retype z)
            TMap   e k v    -> TMap     e (retype k) (retype v)
            THashMap k v    -> THashMap   (retype k) (retype v)

            TType z
                | z == x    -> TType y
                | otherwise -> TType z

    sumValues :: Text
              -> HashMap Text Text
              -> HashMap Text Data
              -> HashMap Text Data
    sumValues k xs = Map.adjust f k
      where
        f (Nullary n ys) = Nullary n (ys <> xs)
        f x              = x

    sumPrefix :: Text
              -> Maybe Text
              -> HashMap Text Data
              -> HashMap Text Data
    sumPrefix _ Nothing  = id
    sumPrefix k (Just y) = Map.adjust f k
      where
        f x@Nullary{} = mapFieldNames (enumName k False y) x
        f x           = x

    required x y f =
        let k = nameCI f
            a = Set.member k x
            b = Set.member k y
         in case f ^. typeOf of
                TMaybe t
                    | a -> f & typeOf .~ t
                    | b -> f
                _   | b -> f & typeOf %~ TMaybe
                _       -> f

    renamed m = nameOf %~ (\n -> fromMaybe n (Map.lookup (CI.mk n) m))

shapes :: Protocol -> Timestamp -> HashMap Text Input.Shape -> HashMap Text Data
shapes proto time m =
    evalState (Map.traverseWithKey solve $ Map.filter skip m) mempty
  where
    skip (Struct' x)
        | Just True <- x ^. scException = False
        | Just True <- x ^. scFault     = False
    skip _                              = True

    solve :: Text -> Input.Shape -> State (HashMap Text Type) Data
    solve k = \case
        Struct' x -> go <$> mapM (field pay req) (ordMap (x ^. scMembers))
          where
            pay = x ^. scPayload
            req = fromMaybe [] (x ^. scRequired)

        String' x
            | Just xs <- x ^. strEnum ->
                return $! Nullary k (Map.fromList (map (join (,)) xs))

        _         -> return Void
      where
        go []  = Void
        go [f] = Newtype k f
        go fs  = Record  k fs

    field :: Maybe Text
          -> [Text]
          -> (Text, Ref)
          -> State (HashMap Text Type) Field
    field pay req (fld, r) = do
        x <- ref fld r
        let s = fromMaybe (stream x) (r ^. refStreaming)
            t = require req fld x
        return Field
            { _fName          = fld
            , _fShape         = r ^. refShape
            , _fType          = t
            , _fLocation      = location proto s (r ^. refLocation)
            , _fLocationName  = fromMaybe fld (r ^. refLocationName)
            , _fPayload       = Just fld == pay
            , _fStream        = s
            , _fDocumentation = above <$> r ^. refDocumentation
            , _fProtocol      = proto
            }

    stream :: Type -> Bool
    stream (TPrim PReq) = True
    stream (TPrim PRes) = True
    stream _            = False

    require :: [Text] -> Text -> Type -> Type
    require req fld x
        | fld `elem` req = x
        | otherwise      =
            case x of
                TPrim      {} -> TMaybe x
                TType      {} -> TMaybe x
                TSensitive {} -> TMaybe x
                _             -> x

    ref :: Text -> Ref -> State (HashMap Text Type) Type
    ref fld r = do
        let k = r ^. refShape
            t = TType k
        mx <- gets (Map.lookup k)
        case mx of
            Just x  -> return x
            Nothing ->
                case Map.lookup k m of
                    Nothing -> insert k t >> return t
                    Just y  -> prop fld r k y

    prop :: Text -> Ref -> Text -> Input.Shape -> State (HashMap Text Type) Type
    prop fld r k s = do
        x <- gets (Map.lookup k)

        -- running 'go' here, ends up at some point selecting the wrong 'StringValueList'
        -- for the EC2.Filter data type, and thereby not having the correct
        -- refLocationName to use - why?

        -- Removed the memoisation due to flattened item problems
        maybe go return x
      where
        go = case s of
            Struct' _ -> pure (TType k)
            Double' _ -> pure (TPrim PDouble)
            Bool'   _ -> pure (TPrim PBool)
            Time'   x -> pure (TPrim . PTime $ fromMaybe time (x ^. tsTimestampFormat))
            Blob'   x
                | Just True <- x ^. blbStreaming -> pure (TPrim PReq)
                | otherwise                      -> pure (TPrim PBlob)

            List'   x -> list x <$> ref fld (x ^. lstMember)
            Map'    x -> hmap x <$> ref fld (x ^. mapKey) <*> ref fld (x ^. mapValue)

            String' x
                | Just _    <- x ^. strEnum      -> pure (TType k)
                | Just True <- x ^. strSensitive -> pure (TSensitive (TPrim PText))
                | otherwise                      -> pure (TPrim PText)

            Int' x
                | isNatural x -> pure (TPrim PNatural)
                | otherwise   -> pure (TPrim PInt)

            Long' x
                | isNatural x -> pure (TPrim PNatural)
                | otherwise   -> pure (TPrim PInteger)

        hmap x k' v'
            | r ^. refLocation == Just Headers = THashMap (TCase k') v'
            | proto == Json                    = THashMap k' v'
            | proto == RestJson                = THashMap k' v'
            | otherwise =
                flat flatten (TMap (ann, key, val) k' v')
          where
            ann | proto == Query          = "entry"
                | fromMaybe False flatten = ent
                | otherwise               = "entry"

            ent = fromMaybe "entry" (r ^. refLocationName)
            key = fromMaybe "key"   (x ^. mapKey   . refLocationName)
            val = fromMaybe "value" (x ^. mapValue . refLocationName)

            flatten = x ^. mapFlattened

        list x = flat flatten . typ ann
          where
            typ | fromMaybe 0 (_lstMin x) > 0 = TList1
                | otherwise                   = TList

            flatten = x ^. lstFlattened
                  <|> x ^. lstMember . refFlattened

            ann | proto == Query = "member"
                | otherwise      =
                    fromMaybe fld $
                            x ^. lstMember . refLocationName
                        <|> r ^. refLocationName

    flat :: Maybe Bool -> Type -> Type
    flat _ | proto == Ec2 = TFlatten
    flat (Just True)      = TFlatten
    flat _                = id

    insert :: Text -> Type -> State (HashMap Text Type) Type
    insert k t = modify (Map.insert k t) >> return t

errorType :: Protocol -> Abbrev -> Text
errorType p a =
    case (p, a) of
        (_,        Abbrev "EC2") -> unAbbrev a <> "Error"
        (Json,     _)            -> "JSONError"
        (RestJson, _)            -> "JSONError"
        _                        -> "RESTError"

prefixWaiters :: HashMap Text Data
              -> HashMap Text Waiter
              -> HashMap Text Waiter
prefixWaiters ds = Map.map go
  where
    go w@Waiter{..} = w & wAcceptors %~ map acceptor
      where
        acceptor a =
            case _aArgument a of
                Nothing -> a
                Just x  ->
                    let (y, e) = runState (prefix initial x) (_aExpected a)
                     in a & aArgument ?~ y
                          & aExpected .~ e

        initial = type' (_wOperation <> "Response")

        prefix :: Data -> Notation -> State Expected Notation
        prefix d = \case
            Indexed k i -> do
                let f = field k d
                Indexed (lensName (_fName f)) <$>
                    prefix (type' (firstName f)) i

            Nested "*" i ->
                Nested "traverse" <$> prefix d i

            Nested  k i -> do
                let f = field k d
                    m = isRequired (f ^. typeOf)
                if m
                   then Nested (lensName (_fName f)) <$>
                       prefix (type' (firstName f)) i
                   else Nested (lensName (_fName f)) . Nested "_Just" <$>
                       prefix (type' (firstName f)) i

            Access  k   -> do
                let f = field k d
                    m = isRequired (f ^. typeOf)
                    r = listToMaybe . mapMaybe name $ universeOn typeOf f
                case type' <$> r of
                    Just (Nullary _ fs) -> do
                        e <- get
                        case e of
                            ExpectText x ->
                                case find ((x ==) . snd) (Map.toList fs) of
                                    Just y  -> put (ExpectCtor (fst y))
                                    Nothing -> error . Text.unpack $ "Failed to find expected text " <> x <> " for field " <> k
                            _ -> return ()
                    _ -> return ()
                return $!
                    if not m
                        then Nested (lensName (_fName f)) (Access "_Just")
                        else Access (lensName (_fName f))

            Bounds k o n -> do
                let f = field k d
                return $! Bounds (lensName (_fName f)) o n

    field k d =
        fromMaybe (error $ "Unable to find field: " ++ show (k, toListOf (dataFields . nameOf) d, Map.keys ds))
                  (findField k d)

    type' k =
        fromMaybe (error $ "Unable to find data type:\n" ++ show (k, Map.keys ds))
                  (Map.lookup k ds)

    firstName f = head . mapMaybe name $ universeOn typeOf f

    name :: Type -> Maybe Text
    name (TType k) = Just k
    name _         = Nothing

findField :: Text -> Data -> Maybe Field
findField (CI.mk -> k) = find f . toListOf dataFields
  where
    f = (k ==) . CI.mk . Text.dropWhile (not . isUpper) . _fName
