{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteTags
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified set of tags from the specified set of resources.
-- This call is designed to follow a 'DescribeTags' request.
--
-- For more information about tags, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging Your Resources>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteTags.html AWS API Reference> for DeleteTags.
module Network.AWS.EC2.DeleteTags
    (
    -- * Creating a Request
      deleteTags
    , DeleteTags
    -- * Request Lenses
    , dtsDryRun
    , dtsTags
    , dtsResources

    -- * Destructuring the Response
    , deleteTagsResponse
    , DeleteTagsResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteTags' smart constructor.
data DeleteTags = DeleteTags'
    { _dtsDryRun    :: !(Maybe Bool)
    , _dtsTags      :: !(Maybe [Tag])
    , _dtsResources :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtsDryRun'
--
-- * 'dtsTags'
--
-- * 'dtsResources'
deleteTags
    :: DeleteTags
deleteTags =
    DeleteTags'
    { _dtsDryRun = Nothing
    , _dtsTags = Nothing
    , _dtsResources = mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
dtsDryRun :: Lens' DeleteTags (Maybe Bool)
dtsDryRun = lens _dtsDryRun (\ s a -> s{_dtsDryRun = a});

-- | One or more tags to delete. If you omit the 'value' parameter, we delete
-- the tag regardless of its value. If you specify this parameter with an
-- empty string as the value, we delete the key only if its value is an
-- empty string.
dtsTags :: Lens' DeleteTags [Tag]
dtsTags = lens _dtsTags (\ s a -> s{_dtsTags = a}) . _Default . _Coerce;

-- | The ID of the resource. For example, ami-1a2b3c4d. You can specify more
-- than one resource ID.
dtsResources :: Lens' DeleteTags [Text]
dtsResources = lens _dtsResources (\ s a -> s{_dtsResources = a}) . _Coerce;

instance AWSRequest DeleteTags where
        type Rs DeleteTags = DeleteTagsResponse
        request = postQuery eC2
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
               "DryRun" =: _dtsDryRun,
               toQuery (toQueryList "Tag" <$> _dtsTags),
               toQueryList "ResourceId" _dtsResources]

-- | /See:/ 'deleteTagsResponse' smart constructor.
data DeleteTagsResponse =
    DeleteTagsResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteTagsResponse' with the minimum fields required to make a request.
--
deleteTagsResponse
    :: DeleteTagsResponse
deleteTagsResponse = DeleteTagsResponse'
