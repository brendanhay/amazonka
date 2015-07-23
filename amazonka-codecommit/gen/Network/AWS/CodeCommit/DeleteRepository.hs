{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.DeleteRepository
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes a repository. If a specified repository was already deleted, a
-- null repository ID will be returned.
--
-- Deleting a repository also deletes all associated objects and metadata.
-- After a repository is deleted, all future push calls to the deleted
-- repository will fail.
--
-- <http://docs.aws.amazon.com/codecommit/latest/APIReference/API_DeleteRepository.html>
module Network.AWS.CodeCommit.DeleteRepository
    (
    -- * Request
      DeleteRepository
    -- ** Request constructor
    , deleteRepository
    -- ** Request lenses
    , drrqRepositoryName

    -- * Response
    , DeleteRepositoryResponse
    -- ** Response constructor
    , deleteRepositoryResponse
    -- ** Response lenses
    , drrsRepositoryId
    , drrsStatus
    ) where

import           Network.AWS.CodeCommit.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a delete repository operation.
--
-- /See:/ 'deleteRepository' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drrqRepositoryName'
newtype DeleteRepository = DeleteRepository'
    { _drrqRepositoryName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteRepository' smart constructor.
deleteRepository :: Text -> DeleteRepository
deleteRepository pRepositoryName_ =
    DeleteRepository'
    { _drrqRepositoryName = pRepositoryName_
    }

-- | The name of the repository to delete.
drrqRepositoryName :: Lens' DeleteRepository Text
drrqRepositoryName = lens _drrqRepositoryName (\ s a -> s{_drrqRepositoryName = a});

instance AWSRequest DeleteRepository where
        type Sv DeleteRepository = CodeCommit
        type Rs DeleteRepository = DeleteRepositoryResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DeleteRepositoryResponse' <$>
                   (x .?> "repositoryId") <*> (pure (fromEnum s)))

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
          = object ["repositoryName" .= _drrqRepositoryName]

instance ToPath DeleteRepository where
        toPath = const "/"

instance ToQuery DeleteRepository where
        toQuery = const mempty

-- | Represents the output of a delete repository operation.
--
-- /See:/ 'deleteRepositoryResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drrsRepositoryId'
--
-- * 'drrsStatus'
data DeleteRepositoryResponse = DeleteRepositoryResponse'
    { _drrsRepositoryId :: !(Maybe Text)
    , _drrsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteRepositoryResponse' smart constructor.
deleteRepositoryResponse :: Int -> DeleteRepositoryResponse
deleteRepositoryResponse pStatus_ =
    DeleteRepositoryResponse'
    { _drrsRepositoryId = Nothing
    , _drrsStatus = pStatus_
    }

-- | The ID of the repository that was deleted.
drrsRepositoryId :: Lens' DeleteRepositoryResponse (Maybe Text)
drrsRepositoryId = lens _drrsRepositoryId (\ s a -> s{_drrsRepositoryId = a});

-- | FIXME: Undocumented member.
drrsStatus :: Lens' DeleteRepositoryResponse Int
drrsStatus = lens _drrsStatus (\ s a -> s{_drrsStatus = a});
