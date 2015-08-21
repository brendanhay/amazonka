{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Gen.AST.Data.Field
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.AST.Data.Field where

import           Control.Applicative
import           Control.Comonad.Cofree
import           Control.Lens
import           Data.Function                (on)
import qualified Data.HashMap.Strict          as Map
import           Data.List                    (elemIndex, sortBy)
import           Data.Maybe
import           Data.Monoid
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Text.Manipulate
import           Gen.AST.TypeOf
import           Gen.Types
import           Language.Haskell.Exts.Syntax (Name (..))

-- | Convenience type to package up some information from the struct with the
-- related field, namely the memberId and the (Set.member required).
data Field = Field
    { _fieldMeta      :: Metadata Identity
    , _fieldOrdinal   :: !Int
    , _fieldId        :: Id    -- ^ The memberId from the struct members map.
    , _fieldRef       :: Ref   -- ^ The original struct member reference.
    , _fieldRequired' :: !Bool -- ^ Does the struct have this member in the required set.
    , _fieldPayload   :: !Bool -- ^ Does the struct have this memeber marked as the payload.
    , _fieldPrefix    :: Maybe Text
    , _fieldNamespace :: Maybe Text
    , _fieldDirection :: Maybe Direction
    }

makeLenses ''Field

instance IsStreaming Field where
    isStreaming = isStreaming . _fieldRef

instance TypeOf Field where
    typeOf f
        | isStreaming r         = t
        | typ, loc            = t
        | f ^. fieldRequired' = t
        | otherwise           = TMaybe t
      where
        t = typeOf r
        r = f ^. fieldRef

        typ = case t of
            TMap  {} -> True
            TList {} -> True
            _        -> False

        loc = fieldLocation f `elem` map Just
            [ Headers
            , Header
            ]

instance HasInfo Field where
    info = fieldAnn . info

-- FIXME: Can just add the metadata to field as well since
-- the protocol/timestamp are passed in everywhere in the .Syntax module.
mkFields :: HasMetadata a Identity
         => a
         -> Solved
         -> StructF (Shape Solved)
         -> [Field]
mkFields (view metadata -> m) s st = sortFields rs $
    zipWith mk [1..] $ Map.toList (st ^. members)
  where
    mk :: Int -> (Id, Ref) -> Field
    mk i (k, v) = Field m i k v req pay p ns d
      where
        req = k `elem` rs
        pay = Just k == st ^. payload

        ns  = (view xmlUri <$> v ^. refXMLNamespace)
          <|> (m ^. xmlNamespace)

    rs = st ^.. getRequired
    p  = s  ^. annPrefix

    d = case s ^. relMode of
        Uni x -> Just x
        Bi    -> Nothing

-- | Ensures that isStreaming fields appear last in the parameter ordering,
-- but doesn't affect the rest of the order which is determined by parsing
-- of the JSON service definition.
sortFields :: [Id] -> [Field] -> [Field]
sortFields xs = zipWith (set fieldOrdinal) [1..]
    -- FIXME: optimise
    . sortBy (on compare isStreaming)
    . sortBy (on compare idx)
  where
    idx x = fromMaybe (-1) $ elemIndex (_fieldId x) xs

fieldAnn :: Lens' Field (Shape Solved)
fieldAnn = fieldRef . refAnn

fieldLens, fieldAccessor :: Field -> Text
fieldLens     f = lensId     (_fieldPrefix f) (_fieldId f)
fieldAccessor f = accessorId (_fieldPrefix f) (_fieldId f)

fieldIsParam :: Field -> Bool
fieldIsParam f = not (fieldMaybe f) && not (fieldMonoid f)

fieldParamName :: Field -> Name
fieldParamName = Ident
    . Text.unpack
    . Text.cons 'p'
    . flip Text.snoc '_'
    . upperHead
    . typeId
    . _fieldId

fieldHelp :: Field -> Help
fieldHelp f =
    fromMaybe def (f ^. fieldRef . refDocumentation) <> ann (typeOf f)
  where
    ann (TMaybe     t) = ann t
    ann (TSensitive t) = ann t
    ann (TLit Blob)    = base64
    ann _              = mempty

    base64 =
        "\n\n/Note:/ This 'Lens' automatically encodes and decodes Base64 data,\n\
        \despite what the AWS documentation might say.\n\
        \The underlying isomorphism will encode to Base64 representation during\n\
        \serialisation, and decode from Base64 representation during deserialisation.\n\
        \This 'Lens' accepts and returns only raw unencoded data."

    def = "Undocumented member."

fieldLocation :: Field -> Maybe Location
fieldLocation = view (fieldRef . refLocation)

fieldBody :: Field -> Bool
fieldBody x =
    case fieldLocation x of
        Just Body -> True
        Nothing   -> True
        _         -> fieldStream x

fieldMaybe :: Field -> Bool
fieldMaybe f =
    case typeOf f of
        TMaybe {} -> True
        _         -> False

fieldMonoid :: Field -> Bool
fieldMonoid = elem DMonoid . derivingOf . view fieldAnn

fieldStream :: Field -> Bool
fieldStream x =
       x ^. fieldRef . refStreaming
    || x ^. fieldAnn . infoStreaming

fieldList1, fieldList, fieldMap :: Field -> Bool
fieldList1 f = fieldList f && nonEmpty f
fieldList    = not . isn't _List . unwrap . view fieldAnn
fieldMap     = not . isn't _Map  . unwrap . view fieldAnn
