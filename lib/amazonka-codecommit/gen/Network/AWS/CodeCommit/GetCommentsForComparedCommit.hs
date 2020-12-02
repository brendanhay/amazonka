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
-- Module      : Network.AWS.CodeCommit.GetCommentsForComparedCommit
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about comments made on the comparison between two commits.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CodeCommit.GetCommentsForComparedCommit
    (
    -- * Creating a Request
      getCommentsForComparedCommit
    , GetCommentsForComparedCommit
    -- * Request Lenses
    , gcfccNextToken
    , gcfccBeforeCommitId
    , gcfccMaxResults
    , gcfccRepositoryName
    , gcfccAfterCommitId

    -- * Destructuring the Response
    , getCommentsForComparedCommitResponse
    , GetCommentsForComparedCommitResponse
    -- * Response Lenses
    , gcfccrsCommentsForComparedCommitData
    , gcfccrsNextToken
    , gcfccrsResponseStatus
    ) where

import Network.AWS.CodeCommit.Types
import Network.AWS.CodeCommit.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCommentsForComparedCommit' smart constructor.
data GetCommentsForComparedCommit = GetCommentsForComparedCommit'
  { _gcfccNextToken      :: !(Maybe Text)
  , _gcfccBeforeCommitId :: !(Maybe Text)
  , _gcfccMaxResults     :: !(Maybe Int)
  , _gcfccRepositoryName :: !Text
  , _gcfccAfterCommitId  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCommentsForComparedCommit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcfccNextToken' - An enumeration token that when provided in a request, returns the next batch of the results.
--
-- * 'gcfccBeforeCommitId' - To establish the directionality of the comparison, the full commit ID of the 'before' commit.
--
-- * 'gcfccMaxResults' - A non-negative integer used to limit the number of returned results. The default is 100 comments, and is configurable up to 500.
--
-- * 'gcfccRepositoryName' - The name of the repository where you want to compare commits.
--
-- * 'gcfccAfterCommitId' - To establish the directionality of the comparison, the full commit ID of the 'after' commit.
getCommentsForComparedCommit
    :: Text -- ^ 'gcfccRepositoryName'
    -> Text -- ^ 'gcfccAfterCommitId'
    -> GetCommentsForComparedCommit
getCommentsForComparedCommit pRepositoryName_ pAfterCommitId_ =
  GetCommentsForComparedCommit'
    { _gcfccNextToken = Nothing
    , _gcfccBeforeCommitId = Nothing
    , _gcfccMaxResults = Nothing
    , _gcfccRepositoryName = pRepositoryName_
    , _gcfccAfterCommitId = pAfterCommitId_
    }


-- | An enumeration token that when provided in a request, returns the next batch of the results.
gcfccNextToken :: Lens' GetCommentsForComparedCommit (Maybe Text)
gcfccNextToken = lens _gcfccNextToken (\ s a -> s{_gcfccNextToken = a})

-- | To establish the directionality of the comparison, the full commit ID of the 'before' commit.
gcfccBeforeCommitId :: Lens' GetCommentsForComparedCommit (Maybe Text)
gcfccBeforeCommitId = lens _gcfccBeforeCommitId (\ s a -> s{_gcfccBeforeCommitId = a})

-- | A non-negative integer used to limit the number of returned results. The default is 100 comments, and is configurable up to 500.
gcfccMaxResults :: Lens' GetCommentsForComparedCommit (Maybe Int)
gcfccMaxResults = lens _gcfccMaxResults (\ s a -> s{_gcfccMaxResults = a})

-- | The name of the repository where you want to compare commits.
gcfccRepositoryName :: Lens' GetCommentsForComparedCommit Text
gcfccRepositoryName = lens _gcfccRepositoryName (\ s a -> s{_gcfccRepositoryName = a})

-- | To establish the directionality of the comparison, the full commit ID of the 'after' commit.
gcfccAfterCommitId :: Lens' GetCommentsForComparedCommit Text
gcfccAfterCommitId = lens _gcfccAfterCommitId (\ s a -> s{_gcfccAfterCommitId = a})

instance AWSPager GetCommentsForComparedCommit where
        page rq rs
          | stop (rs ^. gcfccrsNextToken) = Nothing
          | stop (rs ^. gcfccrsCommentsForComparedCommitData) =
            Nothing
          | otherwise =
            Just $ rq & gcfccNextToken .~ rs ^. gcfccrsNextToken

instance AWSRequest GetCommentsForComparedCommit
         where
        type Rs GetCommentsForComparedCommit =
             GetCommentsForComparedCommitResponse
        request = postJSON codeCommit
        response
          = receiveJSON
              (\ s h x ->
                 GetCommentsForComparedCommitResponse' <$>
                   (x .?> "commentsForComparedCommitData" .!@ mempty)
                     <*> (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable GetCommentsForComparedCommit where

instance NFData GetCommentsForComparedCommit where

instance ToHeaders GetCommentsForComparedCommit where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.GetCommentsForComparedCommit"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetCommentsForComparedCommit where
        toJSON GetCommentsForComparedCommit'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _gcfccNextToken,
                  ("beforeCommitId" .=) <$> _gcfccBeforeCommitId,
                  ("maxResults" .=) <$> _gcfccMaxResults,
                  Just ("repositoryName" .= _gcfccRepositoryName),
                  Just ("afterCommitId" .= _gcfccAfterCommitId)])

instance ToPath GetCommentsForComparedCommit where
        toPath = const "/"

instance ToQuery GetCommentsForComparedCommit where
        toQuery = const mempty

-- | /See:/ 'getCommentsForComparedCommitResponse' smart constructor.
data GetCommentsForComparedCommitResponse = GetCommentsForComparedCommitResponse'
  { _gcfccrsCommentsForComparedCommitData :: !(Maybe [CommentsForComparedCommit])
  , _gcfccrsNextToken :: !(Maybe Text)
  , _gcfccrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCommentsForComparedCommitResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcfccrsCommentsForComparedCommitData' - A list of comment objects on the compared commit.
--
-- * 'gcfccrsNextToken' - An enumeration token that can be used in a request to return the next batch of the results.
--
-- * 'gcfccrsResponseStatus' - -- | The response status code.
getCommentsForComparedCommitResponse
    :: Int -- ^ 'gcfccrsResponseStatus'
    -> GetCommentsForComparedCommitResponse
getCommentsForComparedCommitResponse pResponseStatus_ =
  GetCommentsForComparedCommitResponse'
    { _gcfccrsCommentsForComparedCommitData = Nothing
    , _gcfccrsNextToken = Nothing
    , _gcfccrsResponseStatus = pResponseStatus_
    }


-- | A list of comment objects on the compared commit.
gcfccrsCommentsForComparedCommitData :: Lens' GetCommentsForComparedCommitResponse [CommentsForComparedCommit]
gcfccrsCommentsForComparedCommitData = lens _gcfccrsCommentsForComparedCommitData (\ s a -> s{_gcfccrsCommentsForComparedCommitData = a}) . _Default . _Coerce

-- | An enumeration token that can be used in a request to return the next batch of the results.
gcfccrsNextToken :: Lens' GetCommentsForComparedCommitResponse (Maybe Text)
gcfccrsNextToken = lens _gcfccrsNextToken (\ s a -> s{_gcfccrsNextToken = a})

-- | -- | The response status code.
gcfccrsResponseStatus :: Lens' GetCommentsForComparedCommitResponse Int
gcfccrsResponseStatus = lens _gcfccrsResponseStatus (\ s a -> s{_gcfccrsResponseStatus = a})

instance NFData GetCommentsForComparedCommitResponse
         where
