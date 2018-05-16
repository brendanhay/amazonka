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
-- Module      : Network.AWS.CodeCommit.DeleteRepository
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a repository. If a specified repository was already deleted, a null repository ID will be returned.
--
--
-- /Important:/ Deleting a repository also deletes all associated objects and metadata. After a repository is deleted, all future push calls to the deleted repository will fail.
--
module Network.AWS.CodeCommit.DeleteRepository
    (
    -- * Creating a Request
      deleteRepository
    , DeleteRepository
    -- * Request Lenses
    , drRepositoryName

    -- * Destructuring the Response
    , deleteRepositoryResponse
    , DeleteRepositoryResponse
    -- * Response Lenses
    , drrsRepositoryId
    , drrsResponseStatus
    ) where

import Network.AWS.CodeCommit.Types
import Network.AWS.CodeCommit.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a delete repository operation.
--
--
--
-- /See:/ 'deleteRepository' smart constructor.
newtype DeleteRepository = DeleteRepository'
  { _drRepositoryName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRepository' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drRepositoryName' - The name of the repository to delete.
deleteRepository
    :: Text -- ^ 'drRepositoryName'
    -> DeleteRepository
deleteRepository pRepositoryName_ =
  DeleteRepository' {_drRepositoryName = pRepositoryName_}


-- | The name of the repository to delete.
drRepositoryName :: Lens' DeleteRepository Text
drRepositoryName = lens _drRepositoryName (\ s a -> s{_drRepositoryName = a})

instance AWSRequest DeleteRepository where
        type Rs DeleteRepository = DeleteRepositoryResponse
        request = postJSON codeCommit
        response
          = receiveJSON
              (\ s h x ->
                 DeleteRepositoryResponse' <$>
                   (x .?> "repositoryId") <*> (pure (fromEnum s)))

instance Hashable DeleteRepository where

instance NFData DeleteRepository where

instance ToHeaders DeleteRepository where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.DeleteRepository" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteRepository where
        toJSON DeleteRepository'{..}
          = object
              (catMaybes
                 [Just ("repositoryName" .= _drRepositoryName)])

instance ToPath DeleteRepository where
        toPath = const "/"

instance ToQuery DeleteRepository where
        toQuery = const mempty

-- | Represents the output of a delete repository operation.
--
--
--
-- /See:/ 'deleteRepositoryResponse' smart constructor.
data DeleteRepositoryResponse = DeleteRepositoryResponse'
  { _drrsRepositoryId   :: !(Maybe Text)
  , _drrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRepositoryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drrsRepositoryId' - The ID of the repository that was deleted.
--
-- * 'drrsResponseStatus' - -- | The response status code.
deleteRepositoryResponse
    :: Int -- ^ 'drrsResponseStatus'
    -> DeleteRepositoryResponse
deleteRepositoryResponse pResponseStatus_ =
  DeleteRepositoryResponse'
    {_drrsRepositoryId = Nothing, _drrsResponseStatus = pResponseStatus_}


-- | The ID of the repository that was deleted.
drrsRepositoryId :: Lens' DeleteRepositoryResponse (Maybe Text)
drrsRepositoryId = lens _drrsRepositoryId (\ s a -> s{_drrsRepositoryId = a})

-- | -- | The response status code.
drrsResponseStatus :: Lens' DeleteRepositoryResponse Int
drrsResponseStatus = lens _drrsResponseStatus (\ s a -> s{_drrsResponseStatus = a})

instance NFData DeleteRepositoryResponse where
