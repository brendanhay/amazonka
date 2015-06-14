{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.AutoScaling.CreateOrUpdateTags
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates or updates tags for the specified Auto Scaling group.
--
-- A tag\'s definition is composed of a resource ID, resource type, key and
-- value, and the propagate flag. Value and the propagate flag are optional
-- parameters. See the Request Parameters for more information.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/ASTagging.html Add, Modify, or Remove Auto Scaling Group Tags>
-- in the /Auto Scaling Developer Guide/.
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

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.AutoScaling.Types

-- | /See:/ 'createOrUpdateTags' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'coutTags'
newtype CreateOrUpdateTags = CreateOrUpdateTags'{_coutTags :: [Tag]} deriving (Eq, Read, Show)

-- | 'CreateOrUpdateTags' smart constructor.
createOrUpdateTags :: CreateOrUpdateTags
createOrUpdateTags = CreateOrUpdateTags'{_coutTags = mempty};

-- | The tag to be created or updated. Each tag should be defined by its
-- resource type, resource ID, key, value, and a propagate flag. The
-- resource type and resource ID identify the type and name of resource for
-- which the tag is created. Currently, @auto-scaling-group@ is the only
-- supported resource type. The valid value for the resource ID is
-- /groupname/.
--
-- The @PropagateAtLaunch@ flag defines whether the new tag will be applied
-- to instances launched by the group. Valid values are @true@ or @false@.
-- However, instances that are already running will not get the new or
-- updated tag. Likewise, when you modify a tag, the updated version will
-- be applied only to new instances launched by the group after the change.
-- Running instances that had the previous version of the tag will continue
-- to have the older tag.
--
-- When you create a tag and a tag of the same name already exists, the
-- operation overwrites the previous tag definition, but you will not get
-- an error message.
coutTags :: Lens' CreateOrUpdateTags [Tag]
coutTags = lens _coutTags (\ s a -> s{_coutTags = a});

instance AWSRequest CreateOrUpdateTags where
        type Sv CreateOrUpdateTags = AutoScaling
        type Rs CreateOrUpdateTags =
             CreateOrUpdateTagsResponse
        request = post
        response = receiveNull CreateOrUpdateTagsResponse'

instance ToHeaders CreateOrUpdateTags where
        toHeaders = const mempty

instance ToPath CreateOrUpdateTags where
        toPath = const "/"

instance ToQuery CreateOrUpdateTags where
        toQuery CreateOrUpdateTags'{..}
          = mconcat
              ["Action" =: ("CreateOrUpdateTags" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "Tags" =: "member" =: _coutTags]

-- | /See:/ 'createOrUpdateTagsResponse' smart constructor.
data CreateOrUpdateTagsResponse = CreateOrUpdateTagsResponse' deriving (Eq, Read, Show)

-- | 'CreateOrUpdateTagsResponse' smart constructor.
createOrUpdateTagsResponse :: CreateOrUpdateTagsResponse
createOrUpdateTagsResponse = CreateOrUpdateTagsResponse';
