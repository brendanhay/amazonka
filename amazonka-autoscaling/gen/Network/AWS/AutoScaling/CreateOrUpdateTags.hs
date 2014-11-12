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

-- Module      : Network.AWS.AutoScaling.CreateOrUpdateTags
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates new tags or updates existing tags for an Auto Scaling group. For
-- information on creating tags for your Auto Scaling group, see Tag Your Auto
-- Scaling Groups and Amazon EC2 Instances.
module Network.AWS.AutoScaling.CreateOrUpdateTags
    (
    -- * Request
      CreateOrUpdateTagsType
    -- ** Request constructor
    , createOrUpdateTagsType
    -- ** Request lenses
    , couttTags

    -- * Response
    , CreateOrUpdateTagsResponse
    -- ** Response constructor
    , createOrUpdateTagsResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

newtype CreateOrUpdateTagsType = CreateOrUpdateTagsType
    { _couttTags :: [Tag]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance IsList CreateOrUpdateTagsType
    type Item CreateOrUpdateTagsType = Tag

    fromList = CreateOrUpdateTagsType . fromList
    toList   = toList . _couttTags

-- | 'CreateOrUpdateTagsType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'couttTags' @::@ ['Tag']
--
createOrUpdateTagsType :: CreateOrUpdateTagsType
createOrUpdateTagsType = CreateOrUpdateTagsType
    { _couttTags = mempty
    }

-- | The tag to be created or updated. Each tag should be defined by its
-- resource type, resource ID, key, value, and a propagate flag. The
-- resource type and resource ID identify the type and name of resource for
-- which the tag is created. Currently, auto-scaling-group is the only
-- supported resource type. The valid value for the resource ID is
-- groupname. The PropagateAtLaunch flag defines whether the new tag will be
-- applied to instances launched by the Auto Scaling group. Valid values are
-- true or false. However, instances that are already running will not get
-- the new or updated tag. Likewise, when you modify a tag, the updated
-- version will be applied only to new instances launched by the Auto
-- Scaling group after the change. Running instances that had the previous
-- version of the tag will continue to have the older tag. When you create a
-- tag and a tag of the same name already exists, the operation overwrites
-- the previous tag definition, but you will not get an error message.
couttTags :: Lens' CreateOrUpdateTagsType [Tag]
couttTags = lens _couttTags (\s a -> s { _couttTags = a })

instance ToQuery CreateOrUpdateTagsType

instance ToPath CreateOrUpdateTagsType where
    toPath = const "/"

data CreateOrUpdateTagsResponse = CreateOrUpdateTagsResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'CreateOrUpdateTagsResponse' constructor.
createOrUpdateTagsResponse :: CreateOrUpdateTagsResponse
createOrUpdateTagsResponse = CreateOrUpdateTagsResponse

instance FromXML CreateOrUpdateTagsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateOrUpdateTagsResponse"

instance AWSRequest CreateOrUpdateTagsType where
    type Sv CreateOrUpdateTagsType = AutoScaling
    type Rs CreateOrUpdateTagsType = CreateOrUpdateTagsResponse

    request  = post "CreateOrUpdateTags"
    response = nullaryResponse CreateOrUpdateTagsResponse
