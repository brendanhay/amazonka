{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.CreateOrUpdateTags
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates or updates tags for the specified Auto Scaling group. A tag's
-- definition is composed of a resource ID, resource type, key and value, and
-- the propagate flag. Value and the propagate flag are optional parameters.
-- See the Request Parameters for more information. For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/ASTagging.html
-- Add, Modify, or Remove Auto Scaling Group Tags> in the /Auto Scaling
-- Developer Guide/.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_CreateOrUpdateTags.html>
module Network.AWS.AutoScaling.CreateOrUpdateTags
    (
    -- * Request
      CreateOrUpdateTags
    -- ** Request constructor
    , createOrUpdateTags
    -- ** Request lenses
    , coutTags

    -- * Response
    , CreateOrUpdateTagsResponse
    -- ** Response constructor
    , createOrUpdateTagsResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import qualified GHC.Exts

newtype CreateOrUpdateTags = CreateOrUpdateTags
    { _coutTags :: List "Tags" Tag
    } deriving (Eq, Show, Monoid, Semigroup)

instance GHC.Exts.IsList CreateOrUpdateTags where
    type Item CreateOrUpdateTags = Tag

    fromList = CreateOrUpdateTags . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _coutTags

-- | 'CreateOrUpdateTags' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'coutTags' @::@ ['Tag']
--
createOrUpdateTags :: CreateOrUpdateTags
createOrUpdateTags = CreateOrUpdateTags
    { _coutTags = mempty
    }

-- | The tag to be created or updated. Each tag should be defined by its
-- resource type, resource ID, key, value, and a propagate flag. The
-- resource type and resource ID identify the type and name of resource for
-- which the tag is created. Currently, 'auto-scaling-group' is the only
-- supported resource type. The valid value for the resource ID is
-- /groupname/. The 'PropagateAtLaunch' flag defines whether the new tag
-- will be applied to instances launched by the group. Valid values are
-- 'true' or 'false'. However, instances that are already running will not
-- get the new or updated tag. Likewise, when you modify a tag, the updated
-- version will be applied only to new instances launched by the group after
-- the change. Running instances that had the previous version of the tag
-- will continue to have the older tag. When you create a tag and a tag of
-- the same name already exists, the operation overwrites the previous tag
-- definition, but you will not get an error message.
coutTags :: Lens' CreateOrUpdateTags [Tag]
coutTags = lens _coutTags (\s a -> s { _coutTags = a }) . _List

data CreateOrUpdateTagsResponse = CreateOrUpdateTagsResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'CreateOrUpdateTagsResponse' constructor.
createOrUpdateTagsResponse :: CreateOrUpdateTagsResponse
createOrUpdateTagsResponse = CreateOrUpdateTagsResponse

instance ToPath CreateOrUpdateTags where
    toPath = const "/"

instance ToQuery CreateOrUpdateTags where
    toQuery CreateOrUpdateTags{..} = mconcat
        [ "Tags" =? _coutTags
        ]

instance ToHeaders CreateOrUpdateTags

instance AWSRequest CreateOrUpdateTags where
    type Sv CreateOrUpdateTags = AutoScaling
    type Rs CreateOrUpdateTags = CreateOrUpdateTagsResponse

    request  = post "CreateOrUpdateTags"
    response = nullResponse CreateOrUpdateTagsResponse
