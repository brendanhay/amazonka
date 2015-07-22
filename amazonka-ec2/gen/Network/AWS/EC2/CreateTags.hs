{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateTags
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Adds or overwrites one or more tags for the specified Amazon EC2
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
    , ctrqDryRun
    , ctrqResources
    , ctrqTags

    -- * Response
    , CreateTagsResponse
    -- ** Response constructor
    , createTagsResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createTags' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ctrqDryRun'
--
-- * 'ctrqResources'
--
-- * 'ctrqTags'
data CreateTags = CreateTags'
    { _ctrqDryRun    :: !(Maybe Bool)
    , _ctrqResources :: ![Text]
    , _ctrqTags      :: ![Tag]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateTags' smart constructor.
createTags :: CreateTags
createTags =
    CreateTags'
    { _ctrqDryRun = Nothing
    , _ctrqResources = mempty
    , _ctrqTags = mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
ctrqDryRun :: Lens' CreateTags (Maybe Bool)
ctrqDryRun = lens _ctrqDryRun (\ s a -> s{_ctrqDryRun = a});

-- | The IDs of one or more resources to tag. For example, ami-1a2b3c4d.
ctrqResources :: Lens' CreateTags [Text]
ctrqResources = lens _ctrqResources (\ s a -> s{_ctrqResources = a});

-- | One or more tags. The @value@ parameter is required, but if you don\'t
-- want the tag to have a value, specify the parameter with no value, and
-- we set the value to an empty string.
ctrqTags :: Lens' CreateTags [Tag]
ctrqTags = lens _ctrqTags (\ s a -> s{_ctrqTags = a});

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
               "DryRun" =: _ctrqDryRun,
               toQueryList "ResourceId" _ctrqResources,
               toQueryList "item" _ctrqTags]

-- | /See:/ 'createTagsResponse' smart constructor.
data CreateTagsResponse =
    CreateTagsResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateTagsResponse' smart constructor.
createTagsResponse :: CreateTagsResponse
createTagsResponse = CreateTagsResponse'
