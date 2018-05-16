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
-- Module      : Network.AWS.CodeCommit.GetRepository
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a repository.
--
--
module Network.AWS.CodeCommit.GetRepository
    (
    -- * Creating a Request
      getRepository
    , GetRepository
    -- * Request Lenses
    , grRepositoryName

    -- * Destructuring the Response
    , getRepositoryResponse
    , GetRepositoryResponse
    -- * Response Lenses
    , grrsRepositoryMetadata
    , grrsResponseStatus
    ) where

import Network.AWS.CodeCommit.Types
import Network.AWS.CodeCommit.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a get repository operation.
--
--
--
-- /See:/ 'getRepository' smart constructor.
newtype GetRepository = GetRepository'
  { _grRepositoryName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRepository' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grRepositoryName' - The name of the repository to get information about.
getRepository
    :: Text -- ^ 'grRepositoryName'
    -> GetRepository
getRepository pRepositoryName_ =
  GetRepository' {_grRepositoryName = pRepositoryName_}


-- | The name of the repository to get information about.
grRepositoryName :: Lens' GetRepository Text
grRepositoryName = lens _grRepositoryName (\ s a -> s{_grRepositoryName = a})

instance AWSRequest GetRepository where
        type Rs GetRepository = GetRepositoryResponse
        request = postJSON codeCommit
        response
          = receiveJSON
              (\ s h x ->
                 GetRepositoryResponse' <$>
                   (x .?> "repositoryMetadata") <*> (pure (fromEnum s)))

instance Hashable GetRepository where

instance NFData GetRepository where

instance ToHeaders GetRepository where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.GetRepository" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetRepository where
        toJSON GetRepository'{..}
          = object
              (catMaybes
                 [Just ("repositoryName" .= _grRepositoryName)])

instance ToPath GetRepository where
        toPath = const "/"

instance ToQuery GetRepository where
        toQuery = const mempty

-- | Represents the output of a get repository operation.
--
--
--
-- /See:/ 'getRepositoryResponse' smart constructor.
data GetRepositoryResponse = GetRepositoryResponse'
  { _grrsRepositoryMetadata :: !(Maybe RepositoryMetadata)
  , _grrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRepositoryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grrsRepositoryMetadata' - Information about the repository.
--
-- * 'grrsResponseStatus' - -- | The response status code.
getRepositoryResponse
    :: Int -- ^ 'grrsResponseStatus'
    -> GetRepositoryResponse
getRepositoryResponse pResponseStatus_ =
  GetRepositoryResponse'
    {_grrsRepositoryMetadata = Nothing, _grrsResponseStatus = pResponseStatus_}


-- | Information about the repository.
grrsRepositoryMetadata :: Lens' GetRepositoryResponse (Maybe RepositoryMetadata)
grrsRepositoryMetadata = lens _grrsRepositoryMetadata (\ s a -> s{_grrsRepositoryMetadata = a})

-- | -- | The response status code.
grrsResponseStatus :: Lens' GetRepositoryResponse Int
grrsResponseStatus = lens _grrsResponseStatus (\ s a -> s{_grrsResponseStatus = a})

instance NFData GetRepositoryResponse where
