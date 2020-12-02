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
-- Module      : Network.AWS.EFS.DeleteTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified tags from a file system. If the @DeleteTags@ request includes a tag key that does not exist, Amazon EFS ignores it and doesn't cause an error. For more information about tags and related restrictions, see <http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Tag Restrictions> in the /AWS Billing and Cost Management User Guide/ .
--
--
-- This operation requires permissions for the @elasticfilesystem:DeleteTags@ action.
--
module Network.AWS.EFS.DeleteTags
    (
    -- * Creating a Request
      deleteTags
    , DeleteTags
    -- * Request Lenses
    , dFileSystemId
    , dTagKeys

    -- * Destructuring the Response
    , deleteTagsResponse
    , DeleteTagsResponse
    ) where

import Network.AWS.EFS.Types
import Network.AWS.EFS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'deleteTags' smart constructor.
data DeleteTags = DeleteTags'
  { _dFileSystemId :: !Text
  , _dTagKeys      :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dFileSystemId' - ID of the file system whose tags you want to delete (String).
--
-- * 'dTagKeys' - List of tag keys to delete.
deleteTags
    :: Text -- ^ 'dFileSystemId'
    -> DeleteTags
deleteTags pFileSystemId_ =
  DeleteTags' {_dFileSystemId = pFileSystemId_, _dTagKeys = mempty}


-- | ID of the file system whose tags you want to delete (String).
dFileSystemId :: Lens' DeleteTags Text
dFileSystemId = lens _dFileSystemId (\ s a -> s{_dFileSystemId = a})

-- | List of tag keys to delete.
dTagKeys :: Lens' DeleteTags [Text]
dTagKeys = lens _dTagKeys (\ s a -> s{_dTagKeys = a}) . _Coerce

instance AWSRequest DeleteTags where
        type Rs DeleteTags = DeleteTagsResponse
        request = postJSON efs
        response = receiveNull DeleteTagsResponse'

instance Hashable DeleteTags where

instance NFData DeleteTags where

instance ToHeaders DeleteTags where
        toHeaders = const mempty

instance ToJSON DeleteTags where
        toJSON DeleteTags'{..}
          = object (catMaybes [Just ("TagKeys" .= _dTagKeys)])

instance ToPath DeleteTags where
        toPath DeleteTags'{..}
          = mconcat
              ["/2015-02-01/delete-tags/", toBS _dFileSystemId]

instance ToQuery DeleteTags where
        toQuery = const mempty

-- | /See:/ 'deleteTagsResponse' smart constructor.
data DeleteTagsResponse =
  DeleteTagsResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTagsResponse' with the minimum fields required to make a request.
--
deleteTagsResponse
    :: DeleteTagsResponse
deleteTagsResponse = DeleteTagsResponse'


instance NFData DeleteTagsResponse where
