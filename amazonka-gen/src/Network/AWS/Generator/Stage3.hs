{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Network.AWS.Generator.Stage3
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Generator.Stage3 where

import           Control.Monad
import           Data.String
import qualified Data.Text                    as Text
import           Data.Text.Lazy               (Text)
import qualified Data.Text.Lazy               as LText
import           Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.Encoding      as LText
import           Network.AWS.Generator.Stage2
import           System.FilePath
import           Text.Shakespeare             (RenderUrl)
import           Text.Shakespeare.Text

render :: Service -> Text
render = layout $(textFile "tmpl/module.shakespeare")

-- partial :: a -> Builder
-- partial = $(textFile "templates/partial/package.ehs")

-- searchPackages = layout $(textFile "templates/searchPackages.ehs")
-- searchTags     = layout $(textFile "templates/searchTags.ehs")
-- taglist        = layout $(textFile "templates/taglist.ehs")
-- tag            = layout $(textFile "templates/tag.ehs")

--layout :: IsString c => ((a -> b -> c) -> Builder) -> Text -> Text
layout :: (RenderUrl url -> Builder) -> Service -> Text
layout content Service{..} = toLazyText $
    $(textFileReload "tmpl/_include/layout.shakespeare") renderURL

renderURL :: RenderUrl url
renderURL _ _ = ""
