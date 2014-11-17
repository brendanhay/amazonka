{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CreateTags
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds or overwrites one or more tags for the specified EC2 resource or
-- resources. Each resource can have a maximum of 10 tags. Each tag consists
-- of a key and optional value. Tag keys must be unique per resource. For more
-- information about tags, see Tagging Your Resources in the Amazon Elastic
-- Compute Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateTags.html>
module Network.AWS.EC2.CreateTags
    (
    -- * Request
      CreateTags
    -- ** Request constructor
    , createTags
    -- ** Request lenses
    , ct1DryRun
    , ct1Resources
    , ct1Tags

    -- * Response
    , CreateTagsResponse
    -- ** Response constructor
    , createTagsResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data CreateTags = CreateTags
    { _ct1DryRun    :: Maybe Bool
    , _ct1Resources :: [Text]
    , _ct1Tags      :: [Tag]
    } deriving (Eq, Show, Generic)

-- | 'CreateTags' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ct1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'ct1Resources' @::@ ['Text']
--
-- * 'ct1Tags' @::@ ['Tag']
--
createTags :: CreateTags
createTags = CreateTags
    { _ct1DryRun    = Nothing
    , _ct1Resources = mempty
    , _ct1Tags      = mempty
    }

ct1DryRun :: Lens' CreateTags (Maybe Bool)
ct1DryRun = lens _ct1DryRun (\s a -> s { _ct1DryRun = a })

-- | The IDs of one or more resources to tag. For example, ami-1a2b3c4d.
ct1Resources :: Lens' CreateTags [Text]
ct1Resources = lens _ct1Resources (\s a -> s { _ct1Resources = a })

-- | One or more tags. The value parameter is required, but if you don't want
-- the tag to have a value, specify the parameter with no value, and we set
-- the value to an empty string.
ct1Tags :: Lens' CreateTags [Tag]
ct1Tags = lens _ct1Tags (\s a -> s { _ct1Tags = a })

data CreateTagsResponse = CreateTagsResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'CreateTagsResponse' constructor.
createTagsResponse :: CreateTagsResponse
createTagsResponse = CreateTagsResponse

instance AWSRequest CreateTags where
    type Sv CreateTags = EC2
    type Rs CreateTags = CreateTagsResponse

    request  = post "CreateTags"
    response = nullResponse CreateTagsResponse

instance ToPath CreateTags where
    toPath = const "/"

instance ToHeaders CreateTags

instance ToQuery CreateTags
