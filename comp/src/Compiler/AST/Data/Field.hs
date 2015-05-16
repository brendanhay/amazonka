-- Module      : Compiler.AST.Data.Field
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.AST.Data.Field where

import           Compiler.AST.TypeOf
import           Compiler.Types
import           Control.Lens                 (view)
import qualified Data.Text                    as Text
import           Language.Haskell.Exts.Syntax

data Field = Field
    { fieldId     :: Id
    , fieldType   :: TType
    , fieldDerive :: [Derive]
    , fieldHelp   :: Help
    } deriving (Show)

fieldParam :: Field -> Name
fieldParam = Ident . Text.unpack . view paramId . fieldId

fieldRequire :: Field -> Bool
fieldRequire = requiredType . fieldType

fieldMonoid :: Field -> Bool
fieldMonoid = elem DMonoid . fieldDerive
