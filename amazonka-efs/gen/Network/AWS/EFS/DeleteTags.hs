{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.DeleteTags
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified tags from a file system. If the @DeleteTags@
-- request includes a tag key that does not exist, Amazon EFS ignores it;
-- it is not an error. For more information about tags and related
-- restrictions, go to
-- <http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Tag Restrictions>
-- in the /AWS Billing and Cost Management User Guide/.
--
-- This operation requires permission for the
-- @elasticfilesystem:DeleteTags@ action.
--
-- <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_DeleteTags.html>
module Network.AWS.EFS.DeleteTags
    (
    -- * Request
      DeleteTags
    -- ** Request constructor
    , deleteTags
    -- ** Request lenses
    , dFileSystemId
    , dTagKeys

    -- * Response
    , DeleteTagsResponse
    -- ** Response constructor
    , deleteTagsResponse
    ) where

import           Network.AWS.EFS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteTags' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dFileSystemId'
--
-- * 'dTagKeys'
data DeleteTags = DeleteTags'
    { _dFileSystemId :: !Text
    , _dTagKeys      :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteTags' smart constructor.
deleteTags :: Text -> DeleteTags
deleteTags pFileSystemId_ =
    DeleteTags'
    { _dFileSystemId = pFileSystemId_
    , _dTagKeys = mempty
    }

-- | String. The ID of the file system whose tags you want to delete.
dFileSystemId :: Lens' DeleteTags Text
dFileSystemId = lens _dFileSystemId (\ s a -> s{_dFileSystemId = a});

-- | A list of tag keys to delete.
dTagKeys :: Lens' DeleteTags [Text]
dTagKeys = lens _dTagKeys (\ s a -> s{_dTagKeys = a}) . _Coerce;

instance AWSRequest DeleteTags where
        type Sv DeleteTags = EFS
        type Rs DeleteTags = DeleteTagsResponse
        request = postJSON
        response = receiveNull DeleteTagsResponse'

instance ToHeaders DeleteTags where
        toHeaders = const mempty

instance ToJSON DeleteTags where
        toJSON DeleteTags'{..}
          = object ["TagKeys" .= _dTagKeys]

instance ToPath DeleteTags where
        toPath DeleteTags'{..}
          = mconcat
              ["/2015-02-01/delete-tags/", toPath _dFileSystemId]

instance ToQuery DeleteTags where
        toQuery = const mempty

-- | /See:/ 'deleteTagsResponse' smart constructor.
data DeleteTagsResponse =
    DeleteTagsResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteTagsResponse' smart constructor.
deleteTagsResponse :: DeleteTagsResponse
deleteTagsResponse = DeleteTagsResponse'
