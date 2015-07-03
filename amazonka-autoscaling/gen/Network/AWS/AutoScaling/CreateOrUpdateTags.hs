{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.AutoScaling.CreateOrUpdateTags
-- Copyright   : (c) 2013-2015 Brendan Hay
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
-- A tag is defined by its resource ID, resource type, key, value, and
-- propagate flag. The value and the propagate flag are optional
-- parameters. The only supported resource type is @auto-scaling-group@,
-- and the resource ID must be the name of the group. The
-- @PropagateAtLaunch@ flag determines whether the tag is added to
-- instances launched in the group. Valid values are @true@ or @false@.
--
-- When you specify a tag with a key that already exists, the operation
-- overwrites the previous tag definition, and you do not get an error
-- message.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/ASTagging.html Tagging Auto Scaling Groups and Instances>
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

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createOrUpdateTags' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'coutTags'
newtype CreateOrUpdateTags = CreateOrUpdateTags'
    { _coutTags :: [Tag]
    } deriving (Eq,Read,Show)

-- | 'CreateOrUpdateTags' smart constructor.
createOrUpdateTags :: CreateOrUpdateTags
createOrUpdateTags =
    CreateOrUpdateTags'
    { _coutTags = mempty
    }

-- | One or more tags.
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
               "Tags" =: toQueryList "member" _coutTags]

-- | /See:/ 'createOrUpdateTagsResponse' smart constructor.
data CreateOrUpdateTagsResponse =
    CreateOrUpdateTagsResponse'
    deriving (Eq,Read,Show)

-- | 'CreateOrUpdateTagsResponse' smart constructor.
createOrUpdateTagsResponse :: CreateOrUpdateTagsResponse
createOrUpdateTagsResponse = CreateOrUpdateTagsResponse'
