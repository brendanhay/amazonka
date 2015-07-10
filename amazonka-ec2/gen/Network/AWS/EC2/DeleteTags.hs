{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteTags
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified set of tags from the specified set of resources.
-- This call is designed to follow a @DescribeTags@ request.
--
-- For more information about tags, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging Your Resources>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteTags.html>
module Network.AWS.EC2.DeleteTags
    (
    -- * Request
      DeleteTags
    -- ** Request constructor
    , deleteTags
    -- ** Request lenses
    , delDryRun
    , delTags
    , delResources

    -- * Response
    , DeleteTagsResponse
    -- ** Response constructor
    , deleteTagsResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteTags' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delDryRun'
--
-- * 'delTags'
--
-- * 'delResources'
data DeleteTags = DeleteTags'
    { _delDryRun    :: !(Maybe Bool)
    , _delTags      :: !(Maybe [Tag])
    , _delResources :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteTags' smart constructor.
deleteTags :: DeleteTags
deleteTags =
    DeleteTags'
    { _delDryRun = Nothing
    , _delTags = Nothing
    , _delResources = mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
delDryRun :: Lens' DeleteTags (Maybe Bool)
delDryRun = lens _delDryRun (\ s a -> s{_delDryRun = a});

-- | One or more tags to delete. If you omit the @value@ parameter, we delete
-- the tag regardless of its value. If you specify this parameter with an
-- empty string as the value, we delete the key only if its value is an
-- empty string.
delTags :: Lens' DeleteTags [Tag]
delTags = lens _delTags (\ s a -> s{_delTags = a}) . _Default;

-- | The ID of the resource. For example, ami-1a2b3c4d. You can specify more
-- than one resource ID.
delResources :: Lens' DeleteTags [Text]
delResources = lens _delResources (\ s a -> s{_delResources = a});

instance AWSRequest DeleteTags where
        type Sv DeleteTags = EC2
        type Rs DeleteTags = DeleteTagsResponse
        request = post
        response = receiveNull DeleteTagsResponse'

instance ToHeaders DeleteTags where
        toHeaders = const mempty

instance ToPath DeleteTags where
        toPath = const "/"

instance ToQuery DeleteTags where
        toQuery DeleteTags'{..}
          = mconcat
              ["Action" =: ("DeleteTags" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _delDryRun,
               toQuery (toQueryList "item" <$> _delTags),
               toQueryList "ResourceId" _delResources]

-- | /See:/ 'deleteTagsResponse' smart constructor.
data DeleteTagsResponse =
    DeleteTagsResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteTagsResponse' smart constructor.
deleteTagsResponse :: DeleteTagsResponse
deleteTagsResponse = DeleteTagsResponse'
