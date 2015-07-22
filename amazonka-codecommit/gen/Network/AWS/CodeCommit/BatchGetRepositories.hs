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
    , bgrrqRepositoryNames

    -- * Response
    , BatchGetRepositoriesResponse
    -- ** Response constructor
    , batchGetRepositoriesResponse
    -- ** Response lenses
    , bgrrsRepositories
    , bgrrsRepositoriesNotFound
    , bgrrsStatus
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
-- * 'bgrrqRepositoryNames'
newtype BatchGetRepositories = BatchGetRepositories'
    { _bgrrqRepositoryNames :: [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'BatchGetRepositories' smart constructor.
batchGetRepositories :: BatchGetRepositories
batchGetRepositories =
    BatchGetRepositories'
    { _bgrrqRepositoryNames = mempty
    }

-- | The names of the repositories to get information about.
bgrrqRepositoryNames :: Lens' BatchGetRepositories [Text]
bgrrqRepositoryNames = lens _bgrrqRepositoryNames (\ s a -> s{_bgrrqRepositoryNames = a});

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
          = object ["repositoryNames" .= _bgrrqRepositoryNames]

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
-- * 'bgrrsRepositories'
--
-- * 'bgrrsRepositoriesNotFound'
--
-- * 'bgrrsStatus'
data BatchGetRepositoriesResponse = BatchGetRepositoriesResponse'
    { _bgrrsRepositories         :: !(Maybe [RepositoryMetadata])
    , _bgrrsRepositoriesNotFound :: !(Maybe [Text])
    , _bgrrsStatus               :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'BatchGetRepositoriesResponse' smart constructor.
batchGetRepositoriesResponse :: Int -> BatchGetRepositoriesResponse
batchGetRepositoriesResponse pStatus =
    BatchGetRepositoriesResponse'
    { _bgrrsRepositories = Nothing
    , _bgrrsRepositoriesNotFound = Nothing
    , _bgrrsStatus = pStatus
    }

-- | A list of repositories returned by the batch get repositories operation.
bgrrsRepositories :: Lens' BatchGetRepositoriesResponse [RepositoryMetadata]
bgrrsRepositories = lens _bgrrsRepositories (\ s a -> s{_bgrrsRepositories = a}) . _Default;

-- | Returns a list of repository names for which information could not be
-- found.
bgrrsRepositoriesNotFound :: Lens' BatchGetRepositoriesResponse [Text]
bgrrsRepositoriesNotFound = lens _bgrrsRepositoriesNotFound (\ s a -> s{_bgrrsRepositoriesNotFound = a}) . _Default;

-- | FIXME: Undocumented member.
bgrrsStatus :: Lens' BatchGetRepositoriesResponse Int
bgrrsStatus = lens _bgrrsStatus (\ s a -> s{_bgrrsStatus = a});
