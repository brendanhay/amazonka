{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.CreateTags
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

-- | Adds or overwrites one or more tags for the specified Amazon EC2
-- resource or resources. Each resource can have a maximum of 10 tags. Each
-- tag consists of a key and optional value. Tag keys must be unique per
-- resource.
--
-- For more information about tags, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging Your Resources>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateTags.html>
module Network.AWS.EC2.CreateTags
    (
    -- * Request
      CreateTags
    -- ** Request constructor
    , createTags
    -- ** Request lenses
    , creDryRun
    , creResources
    , creTags

    -- * Response
    , CreateTagsResponse
    -- ** Response constructor
    , createTagsResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createTags' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'creDryRun'
--
-- * 'creResources'
--
-- * 'creTags'
data CreateTags = CreateTags'{_creDryRun :: Maybe Bool, _creResources :: [Text], _creTags :: [Tag]} deriving (Eq, Read, Show)

-- | 'CreateTags' smart constructor.
createTags :: CreateTags
createTags = CreateTags'{_creDryRun = Nothing, _creResources = mempty, _creTags = mempty};

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
creDryRun :: Lens' CreateTags (Maybe Bool)
creDryRun = lens _creDryRun (\ s a -> s{_creDryRun = a});

-- | The IDs of one or more resources to tag. For example, ami-1a2b3c4d.
creResources :: Lens' CreateTags [Text]
creResources = lens _creResources (\ s a -> s{_creResources = a});

-- | One or more tags. The @value@ parameter is required, but if you don\'t
-- want the tag to have a value, specify the parameter with no value, and
-- we set the value to an empty string.
creTags :: Lens' CreateTags [Tag]
creTags = lens _creTags (\ s a -> s{_creTags = a});

instance AWSRequest CreateTags where
        type Sv CreateTags = EC2
        type Rs CreateTags = CreateTagsResponse
        request = post
        response = receiveNull CreateTagsResponse'

instance ToHeaders CreateTags where
        toHeaders = const mempty

instance ToPath CreateTags where
        toPath = const "/"

instance ToQuery CreateTags where
        toQuery CreateTags'{..}
          = mconcat
              ["Action" =: ("CreateTags" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _creDryRun,
               toQueryList "ResourceId" _creResources,
               toQueryList "item" _creTags]

-- | /See:/ 'createTagsResponse' smart constructor.
data CreateTagsResponse = CreateTagsResponse' deriving (Eq, Read, Show)

-- | 'CreateTagsResponse' smart constructor.
createTagsResponse :: CreateTagsResponse
createTagsResponse = CreateTagsResponse';
