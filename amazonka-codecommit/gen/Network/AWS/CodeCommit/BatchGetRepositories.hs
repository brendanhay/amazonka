{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.BatchGetRepositories
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more repositories.
--
-- The description field for a repository accepts all HTML characters and
-- all valid Unicode characters. Applications that do not HTML-encode the
-- description and display it in a web page could expose users to
-- potentially malicious code. Make sure that you HTML-encode the
-- description field in any application that uses this API to display the
-- repository description on a web page.
--
-- <http://docs.aws.amazon.com/codecommit/latest/APIReference/API_BatchGetRepositories.html>
module Network.AWS.CodeCommit.BatchGetRepositories
    (
    -- * Request
      BatchGetRepositories
    -- ** Request constructor
    , batchGetRepositories
    -- ** Request lenses
    , bgrRepositoryNames

    -- * Response
    , BatchGetRepositoriesResponse
    -- ** Response constructor
    , batchGetRepositoriesResponse
    -- ** Response lenses
    , bgrrRepositories
    , bgrrRepositoriesNotFound
    , bgrrStatus
    ) where

import           Network.AWS.CodeCommit.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a batch get repositories operation.
--
-- /See:/ 'batchGetRepositories' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bgrRepositoryNames'
newtype BatchGetRepositories = BatchGetRepositories'
    { _bgrRepositoryNames :: [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'BatchGetRepositories' smart constructor.
batchGetRepositories :: BatchGetRepositories
batchGetRepositories =
    BatchGetRepositories'
    { _bgrRepositoryNames = mempty
    }

-- | The names of the repositories to get information about.
bgrRepositoryNames :: Lens' BatchGetRepositories [Text]
bgrRepositoryNames = lens _bgrRepositoryNames (\ s a -> s{_bgrRepositoryNames = a});

instance AWSRequest BatchGetRepositories where
        type Sv BatchGetRepositories = CodeCommit
        type Rs BatchGetRepositories =
             BatchGetRepositoriesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 BatchGetRepositoriesResponse' <$>
                   (x .?> "repositories" .!@ mempty) <*>
                     (x .?> "repositoriesNotFound" .!@ mempty)
                     <*> (pure (fromEnum s)))

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
          = object ["repositoryNames" .= _bgrRepositoryNames]

instance ToPath BatchGetRepositories where
        toPath = const "/"

instance ToQuery BatchGetRepositories where
        toQuery = const mempty

-- | Represents the output of a batch get repositories operation.
--
-- /See:/ 'batchGetRepositoriesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bgrrRepositories'
--
-- * 'bgrrRepositoriesNotFound'
--
-- * 'bgrrStatus'
data BatchGetRepositoriesResponse = BatchGetRepositoriesResponse'
    { _bgrrRepositories         :: !(Maybe [RepositoryMetadata])
    , _bgrrRepositoriesNotFound :: !(Maybe [Text])
    , _bgrrStatus               :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'BatchGetRepositoriesResponse' smart constructor.
batchGetRepositoriesResponse :: Int -> BatchGetRepositoriesResponse
batchGetRepositoriesResponse pStatus =
    BatchGetRepositoriesResponse'
    { _bgrrRepositories = Nothing
    , _bgrrRepositoriesNotFound = Nothing
    , _bgrrStatus = pStatus
    }

-- | A list of repositories returned by the batch get repositories operation.
bgrrRepositories :: Lens' BatchGetRepositoriesResponse [RepositoryMetadata]
bgrrRepositories = lens _bgrrRepositories (\ s a -> s{_bgrrRepositories = a}) . _Default;

-- | Returns a list of repository names for which information could not be
-- found.
bgrrRepositoriesNotFound :: Lens' BatchGetRepositoriesResponse [Text]
bgrrRepositoriesNotFound = lens _bgrrRepositoriesNotFound (\ s a -> s{_bgrrRepositoriesNotFound = a}) . _Default;

-- | FIXME: Undocumented member.
bgrrStatus :: Lens' BatchGetRepositoriesResponse Int
bgrrStatus = lens _bgrrStatus (\ s a -> s{_bgrrStatus = a});
