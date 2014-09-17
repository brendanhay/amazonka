{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EMR.AddTags
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds tags to an Amazon EMR resource. Tags make it easier to associate
-- clusters in various ways, such as grouping clusters to track your Amazon
-- EMR resource allocation costs. For more information, see Tagging Amazon EMR
-- Resources. POST / HTTP/1.1 Content-Type: application/x-amz-json-1.1
-- X-Amz-Target: ElasticMapReduce.AddTags AUTHPARAMS { "ResourceId":
-- "j-3U7TSX5GZFD8Y", "Tags": [{ "Key": "stack", "Value": "Production" }, {
-- "Key": "hbase" }] } HTTP/1.1 200 OK x-amzn-RequestId:
-- 9da5a349-ed9e-11e2-90db-69a5154aeb8d Content-Type:
-- application/x-amz-json-1.1 Content-Length: 71 Date: Mon, 15 Jul 2013
-- 22:33:47 GMT { }.
module Network.AWS.EMR.AddTags
    (
    -- * Request
      AddTags
    -- ** Request constructor
    , mkAddTags
    -- ** Request lenses
    , atResourceId
    , atTags

    -- * Response
    , AddTagsResponse
    -- ** Response constructor
    , mkAddTagsResponse
    ) where

import Network.AWS.EMR.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | This input identifies a cluster and a list of tags to attach.
data AddTags = AddTags
    { _atResourceId :: Text
    , _atTags :: [Tag]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AddTags' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ResourceId ::@ @Text@
--
-- * @Tags ::@ @[Tag]@
--
mkAddTags :: Text -- ^ 'atResourceId'
          -> [Tag] -- ^ 'atTags'
          -> AddTags
mkAddTags p1 p2 = AddTags
    { _atResourceId = p1
    , _atTags = p2
    }

-- | The Amazon EMR resource identifier to which tags will be added. This value
-- must be a cluster identifier.
atResourceId :: Lens' AddTags Text
atResourceId = lens _atResourceId (\s a -> s { _atResourceId = a })

-- | A list of tags to associate with a cluster and propagate to Amazon EC2
-- instances. Tags are user-defined key/value pairs that consist of a required
-- key string with a maximum of 128 characters, and an optional value string
-- with a maximum of 256 characters.
atTags :: Lens' AddTags [Tag]
atTags = lens _atTags (\s a -> s { _atTags = a })

instance ToPath AddTags

instance ToQuery AddTags

instance ToHeaders AddTags

instance ToJSON AddTags

-- | This output indicates the result of adding tags to a resource.
data AddTagsResponse = AddTagsResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AddTagsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkAddTagsResponse :: AddTagsResponse
mkAddTagsResponse = AddTagsResponse

instance AWSRequest AddTags where
    type Sv AddTags = EMR
    type Rs AddTags = AddTagsResponse

    request = get
    response _ = nullaryResponse AddTagsResponse
