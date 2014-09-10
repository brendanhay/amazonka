{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EMR
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
module Network.AWS.EMR
    (
    -- * Request
      RemoveTags
    -- ** Request constructor
    , mkRemoveTags
    -- ** Request lenses
    , rtResourceId
    , rtTagKeys

    -- * Response
    , RemoveTagsResponse
    -- ** Response constructor
    , mkRemoveTagsResponse
    ) where

import Network.AWS.EMR.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | This input identifies a cluster and a list of tags to remove.
data RemoveTags = RemoveTags
    { _rtResourceId :: !Text
    , _rtTagKeys :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RemoveTags' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ResourceId ::@ @Text@
--
-- * @TagKeys ::@ @[Text]@
--
mkRemoveTags :: Text -- ^ 'rtResourceId'
             -> [Text] -- ^ 'rtTagKeys'
             -> RemoveTags
mkRemoveTags p1 p2 = RemoveTags
    { _rtResourceId = p1
    , _rtTagKeys = p2
    }

-- | The Amazon EMR resource identifier from which tags will be removed. This
-- value must be a cluster identifier.
rtResourceId :: Lens' RemoveTags Text
rtResourceId = lens _rtResourceId (\s a -> s { _rtResourceId = a })

-- | A list of tag keys to remove from a resource.
rtTagKeys :: Lens' RemoveTags [Text]
rtTagKeys = lens _rtTagKeys (\s a -> s { _rtTagKeys = a })

instance ToPath RemoveTags

instance ToQuery RemoveTags

instance ToHeaders RemoveTags

instance ToJSON RemoveTags

-- | This output indicates the result of removing tags from a resource.
data RemoveTagsResponse = RemoveTagsResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RemoveTagsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkRemoveTagsResponse :: RemoveTagsResponse
mkRemoveTagsResponse = RemoveTagsResponse

instance AWSRequest RemoveTags where
    type Sv RemoveTags = EMR
    type Rs RemoveTags = RemoveTagsResponse

    request = get
    response _ = nullaryResponse RemoveTagsResponse
