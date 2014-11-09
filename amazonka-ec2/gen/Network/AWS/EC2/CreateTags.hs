{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
module Network.AWS.EC2.CreateTags
    (
    -- * Request
      CreateTags
    -- ** Request constructor
    , createTags
    -- ** Request lenses
    , ctDryRun
    , ctResources
    , ctTags

    -- * Response
    , CreateTagsResponse
    -- ** Response constructor
    , createTagsResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data CreateTags = CreateTags
    { _ctDryRun    :: Maybe Bool
    , _ctResources :: [Text]
    , _ctTags      :: [Tag]
    } deriving (Eq, Show, Generic)

-- | 'CreateTags' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ctDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'ctResources' @::@ ['Text']
--
-- * 'ctTags' @::@ ['Tag']
--
createTags :: CreateTags
createTags = CreateTags
    { _ctDryRun    = Nothing
    , _ctResources = mempty
    , _ctTags      = mempty
    }

ctDryRun :: Lens' CreateTags (Maybe Bool)
ctDryRun = lens _ctDryRun (\s a -> s { _ctDryRun = a })

-- | The IDs of one or more resources to tag. For example, ami-1a2b3c4d.
ctResources :: Lens' CreateTags [Text]
ctResources = lens _ctResources (\s a -> s { _ctResources = a })

-- | One or more tags. The value parameter is required, but if you don't want
-- the tag to have a value, specify the parameter with no value, and we set
-- the value to an empty string.
ctTags :: Lens' CreateTags [Tag]
ctTags = lens _ctTags (\s a -> s { _ctTags = a })

instance ToPath CreateTags where
    toPath = const "/"

instance ToQuery CreateTags

data CreateTagsResponse = CreateTagsResponse

-- | 'CreateTagsResponse' constructor.
createTagsResponse :: CreateTagsResponse
createTagsResponse = CreateTagsResponse

instance AWSRequest CreateTags where
    type Sv CreateTags = EC2
    type Rs CreateTags = CreateTagsResponse

    request  = post "CreateTags"
    response = const (nullaryResponse CreateTagsResponse)
