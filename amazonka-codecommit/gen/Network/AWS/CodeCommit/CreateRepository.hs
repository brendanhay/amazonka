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
-- Module      : Network.AWS.CodeCommit.CreateRepository
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new, empty repository.
--
-- /See:/ <http://docs.aws.amazon.com/codecommit/latest/APIReference/API_CreateRepository.html AWS API Reference> for CreateRepository.
module Network.AWS.CodeCommit.CreateRepository
    (
    -- * Creating a Request
      CreateRepository
    , createRepository
    -- * Request Lenses
    , crRepositoryDescription
    , crRepositoryName

    -- * Destructuring the Response
    , CreateRepositoryResponse
    , createRepositoryResponse
    -- * Response Lenses
    , crrsRepositoryMetadata
    , crrsStatus
    ) where

import Network.AWS.CodeCommit.Types
import Network.AWS.CodeCommit.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a create repository operation.
--
-- /See:/ 'createRepository' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crRepositoryDescription'
--
-- * 'crRepositoryName'
data CreateRepository = CreateRepository'
    { _crRepositoryDescription :: !(Maybe Text)
    , _crRepositoryName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateRepository' smart constructor.
createRepository :: Text -> CreateRepository
createRepository pRepositoryName_ = 
    CreateRepository'
    { _crRepositoryDescription = Nothing
    , _crRepositoryName = pRepositoryName_
    }

-- | A comment or description about the new repository.
crRepositoryDescription :: Lens' CreateRepository (Maybe Text)
crRepositoryDescription = lens _crRepositoryDescription (\ s a -> s{_crRepositoryDescription = a});

-- | The name of the new repository to be created.
--
-- The repository name must be unique across the calling AWS account. In
-- addition, repository names are restricted to alphanumeric characters.
-- The suffix \".git\" is prohibited.
crRepositoryName :: Lens' CreateRepository Text
crRepositoryName = lens _crRepositoryName (\ s a -> s{_crRepositoryName = a});

instance AWSRequest CreateRepository where
        type Sv CreateRepository = CodeCommit
        type Rs CreateRepository = CreateRepositoryResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateRepositoryResponse' <$>
                   (x .?> "repositoryMetadata") <*> (pure (fromEnum s)))

instance ToHeaders CreateRepository where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.CreateRepository" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateRepository where
        toJSON CreateRepository'{..}
          = object
              ["repositoryDescription" .= _crRepositoryDescription,
               "repositoryName" .= _crRepositoryName]

instance ToPath CreateRepository where
        toPath = const "/"

instance ToQuery CreateRepository where
        toQuery = const mempty

-- | Represents the output of a create repository operation.
--
-- /See:/ 'createRepositoryResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crrsRepositoryMetadata'
--
-- * 'crrsStatus'
data CreateRepositoryResponse = CreateRepositoryResponse'
    { _crrsRepositoryMetadata :: !(Maybe RepositoryMetadata)
    , _crrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateRepositoryResponse' smart constructor.
createRepositoryResponse :: Int -> CreateRepositoryResponse
createRepositoryResponse pStatus_ = 
    CreateRepositoryResponse'
    { _crrsRepositoryMetadata = Nothing
    , _crrsStatus = pStatus_
    }

-- | Information about the newly created repository.
crrsRepositoryMetadata :: Lens' CreateRepositoryResponse (Maybe RepositoryMetadata)
crrsRepositoryMetadata = lens _crrsRepositoryMetadata (\ s a -> s{_crrsRepositoryMetadata = a});

-- | Undocumented member.
crrsStatus :: Lens' CreateRepositoryResponse Int
crrsStatus = lens _crrsStatus (\ s a -> s{_crrsStatus = a});
