{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.GetRepository
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a repository.
--
-- The description field for a repository accepts all HTML characters and
-- all valid Unicode characters. Applications that do not HTML-encode the
-- description and display it in a web page could expose users to
-- potentially malicious code. Make sure that you HTML-encode the
-- description field in any application that uses this API to display the
-- repository description on a web page.
--
-- <http://docs.aws.amazon.com/codecommit/latest/APIReference/API_GetRepository.html>
module Network.AWS.CodeCommit.GetRepository
    (
    -- * Request
      GetRepository
    -- ** Request constructor
    , getRepository
    -- ** Request lenses
    , grRepositoryName

    -- * Response
    , GetRepositoryResponse
    -- ** Response constructor
    , getRepositoryResponse
    -- ** Response lenses
    , grrsRepositoryMetadata
    , grrsStatus
    ) where

import           Network.AWS.CodeCommit.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a get repository operation.
--
-- /See:/ 'getRepository' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'grRepositoryName'
newtype GetRepository = GetRepository'
    { _grRepositoryName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetRepository' smart constructor.
getRepository :: Text -> GetRepository
getRepository pRepositoryName_ =
    GetRepository'
    { _grRepositoryName = pRepositoryName_
    }

-- | The name of the repository to get information about.
grRepositoryName :: Lens' GetRepository Text
grRepositoryName = lens _grRepositoryName (\ s a -> s{_grRepositoryName = a});

instance AWSRequest GetRepository where
        type Sv GetRepository = CodeCommit
        type Rs GetRepository = GetRepositoryResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetRepositoryResponse' <$>
                   (x .?> "repositoryMetadata") <*> (pure (fromEnum s)))

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
          = object ["repositoryName" .= _grRepositoryName]

instance ToPath GetRepository where
        toPath = const "/"

instance ToQuery GetRepository where
        toQuery = const mempty

-- | Represents the output of a get repository operation.
--
-- /See:/ 'getRepositoryResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'grrsRepositoryMetadata'
--
-- * 'grrsStatus'
data GetRepositoryResponse = GetRepositoryResponse'
    { _grrsRepositoryMetadata :: !(Maybe RepositoryMetadata)
    , _grrsStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetRepositoryResponse' smart constructor.
getRepositoryResponse :: Int -> GetRepositoryResponse
getRepositoryResponse pStatus_ =
    GetRepositoryResponse'
    { _grrsRepositoryMetadata = Nothing
    , _grrsStatus = pStatus_
    }

-- | Information about the repository.
grrsRepositoryMetadata :: Lens' GetRepositoryResponse (Maybe RepositoryMetadata)
grrsRepositoryMetadata = lens _grrsRepositoryMetadata (\ s a -> s{_grrsRepositoryMetadata = a});

-- | FIXME: Undocumented member.
grrsStatus :: Lens' GetRepositoryResponse Int
grrsStatus = lens _grrsStatus (\ s a -> s{_grrsStatus = a});
