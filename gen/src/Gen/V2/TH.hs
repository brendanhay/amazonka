{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Gen.V2.TH
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.V2.TH where

import           Control.Applicative
import           Control.Lens
import           Data.Jason.TH
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Text.Manipulate
import           Gen.V2.Naming
import           Language.Haskell.TH

data TH = TH
    { _thCtor  :: Text -> Text
    , _thField :: Text -> Text
    , _thLens  :: Text -> Text
    , _thJSON  :: Name -> Q [Dec]
    }

makeLenses ''TH

stage1, stage2 :: TH
stage1 = TH toSpinal keyName lensName (deriveFromJSON (aeson stage1))
stage2 = TH toSpinal keyName lensName (deriveToJSON   (aeson stage2))

nullary :: TH -> Name -> Q [Dec]
nullary = _thJSON

record :: TH -> Name -> Q [Dec]
record th n = concat <$> sequence
    [ makeLensesWith (lenses th lensRules) n
    , _thJSON th n
    ]

classy :: TH -> Name -> Q [Dec]
classy th n = concat <$> sequence
    [ makeLensesWith (lenses th classyRules) n
    , _thJSON th n
    ]

aeson :: TH -> Options
aeson th = defaultOptions
    { constructorTagModifier = text (_thCtor th)
    , fieldLabelModifier     = text (_thField th)
    , omitNothingFields      = True
    , allNullaryToStringTag  = True
    }

lenses :: TH -> LensRules -> LensRules
lenses th = lensField .~ const f
  where
    f :: Name -> [DefName]
    f x = [TopName (mkName (text (_thLens th) (nameBase x)))]

text :: (Text -> Text) -> String -> String
text f = Text.unpack . f . Text.pack
