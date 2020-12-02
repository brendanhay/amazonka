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
-- Module      : Network.AWS.CodeCommit.BatchGetRepositories
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about one or more repositories.
--
--
module Network.AWS.CodeCommit.BatchGetRepositories
    (
    -- * Creating a Request
      batchGetRepositories
    , BatchGetRepositories
    -- * Request Lenses
    , bgrRepositoryNames

    -- * Destructuring the Response
    , batchGetRepositoriesResponse
    , BatchGetRepositoriesResponse
    -- * Response Lenses
    , bgrrsRepositories
    , bgrrsRepositoriesNotFound
    , bgrrsResponseStatus
    ) where

import Network.AWS.CodeCommit.Types
import Network.AWS.CodeCommit.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a batch get repositories operation.
--
--
--
-- /See:/ 'batchGetRepositories' smart constructor.
newtype BatchGetRepositories = BatchGetRepositories'
  { _bgrRepositoryNames :: [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchGetRepositories' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgrRepositoryNames' - The names of the repositories to get information about.
batchGetRepositories
    :: BatchGetRepositories
batchGetRepositories = BatchGetRepositories' {_bgrRepositoryNames = mempty}


-- | The names of the repositories to get information about.
bgrRepositoryNames :: Lens' BatchGetRepositories [Text]
bgrRepositoryNames = lens _bgrRepositoryNames (\ s a -> s{_bgrRepositoryNames = a}) . _Coerce

instance AWSRequest BatchGetRepositories where
        type Rs BatchGetRepositories =
             BatchGetRepositoriesResponse
        request = postJSON codeCommit
        response
          = receiveJSON
              (\ s h x ->
                 BatchGetRepositoriesResponse' <$>
                   (x .?> "repositories" .!@ mempty) <*>
                     (x .?> "repositoriesNotFound" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable BatchGetRepositories where

instance NFData BatchGetRepositories where

instance ToHeaders BatchGetRepositories where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.BatchGetRepositories" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON BatchGetRepositories where
        toJSON BatchGetRepositories'{..}
          = object
              (catMaybes
                 [Just ("repositoryNames" .= _bgrRepositoryNames)])

instance ToPath BatchGetRepositories where
        toPath = const "/"

instance ToQuery BatchGetRepositories where
        toQuery = const mempty

-- | Represents the output of a batch get repositories operation.
--
--
--
-- /See:/ 'batchGetRepositoriesResponse' smart constructor.
data BatchGetRepositoriesResponse = BatchGetRepositoriesResponse'
  { _bgrrsRepositories         :: !(Maybe [RepositoryMetadata])
  , _bgrrsRepositoriesNotFound :: !(Maybe [Text])
  , _bgrrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchGetRepositoriesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgrrsRepositories' - A list of repositories returned by the batch get repositories operation.
--
-- * 'bgrrsRepositoriesNotFound' - Returns a list of repository names for which information could not be found.
--
-- * 'bgrrsResponseStatus' - -- | The response status code.
batchGetRepositoriesResponse
    :: Int -- ^ 'bgrrsResponseStatus'
    -> BatchGetRepositoriesResponse
batchGetRepositoriesResponse pResponseStatus_ =
  BatchGetRepositoriesResponse'
    { _bgrrsRepositories = Nothing
    , _bgrrsRepositoriesNotFound = Nothing
    , _bgrrsResponseStatus = pResponseStatus_
    }


-- | A list of repositories returned by the batch get repositories operation.
bgrrsRepositories :: Lens' BatchGetRepositoriesResponse [RepositoryMetadata]
bgrrsRepositories = lens _bgrrsRepositories (\ s a -> s{_bgrrsRepositories = a}) . _Default . _Coerce

-- | Returns a list of repository names for which information could not be found.
bgrrsRepositoriesNotFound :: Lens' BatchGetRepositoriesResponse [Text]
bgrrsRepositoriesNotFound = lens _bgrrsRepositoriesNotFound (\ s a -> s{_bgrrsRepositoriesNotFound = a}) . _Default . _Coerce

-- | -- | The response status code.
bgrrsResponseStatus :: Lens' BatchGetRepositoriesResponse Int
bgrrsResponseStatus = lens _bgrrsResponseStatus (\ s a -> s{_bgrrsResponseStatus = a})

instance NFData BatchGetRepositoriesResponse where
