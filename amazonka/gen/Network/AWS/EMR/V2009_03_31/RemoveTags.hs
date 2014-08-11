{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EMR.V2009_03_31.RemoveTags
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes tags from an Amazon EMR resource. Tags make it easier to associate
-- clusters in various ways, such as grouping clusters to track your Amazon
-- EMR resource allocation costs. For more information, see Tagging Amazon EMR
-- Resources. The following example removes the stack tag with value Prod from
-- a cluster: POST / HTTP/1.1 Content-Type: application/x-amz-json-1.1
-- X-Amz-Target: ElasticMapReduce.RemoveTags AUTHPARAMS { "ResourceId":
-- "j-3U7TSX5GZFD8Y", "Tags": [{ "Key": "stack", "Value": "Prod" }] } HTTP/1.1
-- 200 OK x-amzn-RequestId: 9da5a349-ed9e-11e2-90db-69a5154aeb8d Content-Type:
-- application/x-amz-json-1.1 Content-Length: 71 Date: Mon, 15 Jul 2013
-- 22:33:47 GMT { } The following example removes the stack and hbase tags
-- from a cluster: POST / HTTP/1.1 Content-Type: application/x-amz-json-1.1
-- X-Amz-Target: ElasticMapReduce.RemoveTags AUTHPARAMS { "ResourceId":
-- "j-3U7TSX5GZFD8Y", "Tags": [{ "Key": "stack" }, { "Key": "hbase" }] }
-- HTTP/1.1 200 OK x-amzn-RequestId: 9da5a349-ed9e-11e2-90db-69a5154aeb8d
-- Content-Type: application/x-amz-json-1.1 Content-Length: 71 Date: Mon, 15
-- Jul 2013 22:33:47 GMT { }.
module Network.AWS.EMR.V2009_03_31.RemoveTags where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.EMR.V2009_03_31.Types
import Network.AWS.Prelude

data RemoveTags = RemoveTags
    { _rtiResourceId :: Text
      -- ^ The Amazon EMR resource identifier from which tags will be
      -- removed. This value must be a cluster identifier.
    , _rtiTagKeys :: [Text]
      -- ^ A list of tag keys to remove from a resource.
    } deriving (Show, Generic)

makeLenses ''RemoveTags

instance ToPath RemoveTags

instance ToQuery RemoveTags

instance ToHeaders RemoveTags

instance ToJSON RemoveTags

data RemoveTagsResponse = RemoveTagsResponse
    deriving (Eq, Show, Generic)

makeLenses ''RemoveTagsResponse

instance AWSRequest RemoveTags where
    type Sv RemoveTags = EMR
    type Rs RemoveTags = RemoveTagsResponse

    request = get
    response _ = nullaryResponse RemoveTagsResponse
