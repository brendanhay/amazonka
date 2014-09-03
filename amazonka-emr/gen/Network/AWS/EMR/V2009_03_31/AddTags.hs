{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EMR.V2009_03_31.AddTags
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
module Network.AWS.EMR.V2009_03_31.AddTags
    (
    -- * Request
      AddTags
    -- ** Request constructor
    , addTags
    -- ** Request lenses
    , atiResourceId
    , atiTags

    -- * Response
    , AddTagsResponse
    ) where

import           Network.AWS.EMR.V2009_03_31.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'AddTags' request.
addTags :: Text -- ^ 'atiResourceId'
        -> [Tag] -- ^ 'atiTags'
        -> AddTags
addTags p1 p2 = AddTags
    { _atiResourceId = p1
    , _atiTags = p2
    }

data AddTags = AddTags
    { _atiResourceId :: Text
      -- ^ The Amazon EMR resource identifier to which tags will be added.
      -- This value must be a cluster identifier.
    , _atiTags :: [Tag]
      -- ^ A list of tags to associate with a cluster and propagate to
      -- Amazon EC2 instances. Tags are user-defined key/value pairs that
      -- consist of a required key string with a maximum of 128
      -- characters, and an optional value string with a maximum of 256
      -- characters.
    } deriving (Show, Generic)

-- | The Amazon EMR resource identifier to which tags will be added. This value
-- must be a cluster identifier.
atiResourceId
    :: Functor f
    => (Text
    -> f (Text))
    -> AddTags
    -> f AddTags
atiResourceId f x =
    (\y -> x { _atiResourceId = y })
       <$> f (_atiResourceId x)
{-# INLINE atiResourceId #-}

-- | A list of tags to associate with a cluster and propagate to Amazon EC2
-- instances. Tags are user-defined key/value pairs that consist of a required
-- key string with a maximum of 128 characters, and an optional value string
-- with a maximum of 256 characters.
atiTags
    :: Functor f
    => ([Tag]
    -> f ([Tag]))
    -> AddTags
    -> f AddTags
atiTags f x =
    (\y -> x { _atiTags = y })
       <$> f (_atiTags x)
{-# INLINE atiTags #-}

instance ToPath AddTags

instance ToQuery AddTags

instance ToHeaders AddTags

instance ToJSON AddTags

data AddTagsResponse = AddTagsResponse
    deriving (Eq, Show, Generic)

instance AWSRequest AddTags where
    type Sv AddTags = EMR
    type Rs AddTags = AddTagsResponse

    request = get
    response _ = nullaryResponse AddTagsResponse
