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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new, empty repository.
module Network.AWS.CodeCommit.CreateRepository
    (
    -- * Creating a Request
      createRepository
    , CreateRepository
    -- * Request Lenses
    , crRepositoryDescription
    , crRepositoryName

    -- * Destructuring the Response
    , createRepositoryResponse
    , CreateRepositoryResponse
    -- * Response Lenses
    , crrsRepositoryMetadata
    , crrsResponseStatus
    ) where

import           Network.AWS.CodeCommit.Types
import           Network.AWS.CodeCommit.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a create repository operation.
--
-- /See:/ 'createRepository' smart constructor.
data CreateRepository = CreateRepository'
    { _crRepositoryDescription :: !(Maybe Text)
    , _crRepositoryName        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateRepository' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crRepositoryDescription'
--
-- * 'crRepositoryName'
createRepository
    :: Text -- ^ 'crRepositoryName'
    -> CreateRepository
createRepository pRepositoryName_ =
    CreateRepository'
    { _crRepositoryDescription = Nothing
    , _crRepositoryName = pRepositoryName_
    }

-- | A comment or description about the new repository.
--
-- The description field for a repository accepts all HTML characters and all valid Unicode characters. Applications that do not HTML-encode the description and display it in a web page could expose users to potentially malicious code. Make sure that you HTML-encode the description field in any application that uses this API to display the repository description on a web page.
crRepositoryDescription :: Lens' CreateRepository (Maybe Text)
crRepositoryDescription = lens _crRepositoryDescription (\ s a -> s{_crRepositoryDescription = a});

-- | The name of the new repository to be created.
--
-- The repository name must be unique across the calling AWS account. In addition, repository names are limited to 100 alphanumeric, dash, and underscore characters, and cannot include certain characters. For a full description of the limits on repository names, see <http://docs.aws.amazon.com/codecommit/latest/userguide/limits.html Limits> in the AWS CodeCommit User Guide. The suffix \".git\" is prohibited.
crRepositoryName :: Lens' CreateRepository Text
crRepositoryName = lens _crRepositoryName (\ s a -> s{_crRepositoryName = a});

instance AWSRequest CreateRepository where
        type Rs CreateRepository = CreateRepositoryResponse
        request = postJSON codeCommit
        response
          = receiveJSON
              (\ s h x ->
                 CreateRepositoryResponse' <$>
                   (x .?> "repositoryMetadata") <*> (pure (fromEnum s)))

instance Hashable CreateRepository

instance NFData CreateRepository

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
              (catMaybes
                 [("repositoryDescription" .=) <$>
                    _crRepositoryDescription,
                  Just ("repositoryName" .= _crRepositoryName)])

instance ToPath CreateRepository where
        toPath = const "/"

instance ToQuery CreateRepository where
        toQuery = const mempty

-- | Represents the output of a create repository operation.
--
-- /See:/ 'createRepositoryResponse' smart constructor.
data CreateRepositoryResponse = CreateRepositoryResponse'
    { _crrsRepositoryMetadata :: !(Maybe RepositoryMetadata)
    , _crrsResponseStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateRepositoryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crrsRepositoryMetadata'
--
-- * 'crrsResponseStatus'
createRepositoryResponse
    :: Int -- ^ 'crrsResponseStatus'
    -> CreateRepositoryResponse
createRepositoryResponse pResponseStatus_ =
    CreateRepositoryResponse'
    { _crrsRepositoryMetadata = Nothing
    , _crrsResponseStatus = pResponseStatus_
    }

-- | Information about the newly created repository.
crrsRepositoryMetadata :: Lens' CreateRepositoryResponse (Maybe RepositoryMetadata)
crrsRepositoryMetadata = lens _crrsRepositoryMetadata (\ s a -> s{_crrsRepositoryMetadata = a});

-- | The response status code.
crrsResponseStatus :: Lens' CreateRepositoryResponse Int
crrsResponseStatus = lens _crrsResponseStatus (\ s a -> s{_crrsResponseStatus = a});

instance NFData CreateRepositoryResponse
