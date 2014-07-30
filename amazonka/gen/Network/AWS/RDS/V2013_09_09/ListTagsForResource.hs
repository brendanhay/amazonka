{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.ListTagsForResource
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists all tags on an Amazon RDS resource. For an overview on tagging an
-- Amazon RDS resource, see Tagging Amazon RDS Resources.
module Network.AWS.RDS.V2013_09_09.ListTagsForResource where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.RDS.V2013_09_09.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data ListTagsForResource = ListTagsForResource
    { _ltfrmResourceName :: Text
      -- ^ The Amazon RDS resource with tags to be listed. This value is an
      -- Amazon Resource Name (ARN). For information about creating an
      -- ARN, see Constructing an RDS Amazon Resource Name (ARN).
    } deriving (Generic)

instance ToQuery ListTagsForResource where
    toQuery = genericToQuery def

instance AWSRequest ListTagsForResource where
    type Sv ListTagsForResource = RDS
    type Rs ListTagsForResource = ListTagsForResourceResponse

    request = post "ListTagsForResource"
    response _ = xmlResponse

data ListTagsForResourceResponse = ListTagsForResourceResponse
    { _tlmTagList :: [Tag]
      -- ^ List of tags returned by the ListTagsForResource operation.
    } deriving (Generic)

instance FromXML ListTagsForResourceResponse where
    fromXMLOptions = xmlOptions
