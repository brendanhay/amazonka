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
-- Module      : Network.AWS.CodeCommit.PostCommentForComparedCommit
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Posts a comment on the comparison between two commits.
--
--
module Network.AWS.CodeCommit.PostCommentForComparedCommit
    (
    -- * Creating a Request
      postCommentForComparedCommit
    , PostCommentForComparedCommit
    -- * Request Lenses
    , pcfccLocation
    , pcfccBeforeCommitId
    , pcfccClientRequestToken
    , pcfccRepositoryName
    , pcfccAfterCommitId
    , pcfccContent

    -- * Destructuring the Response
    , postCommentForComparedCommitResponse
    , PostCommentForComparedCommitResponse
    -- * Response Lenses
    , pcfccrsBeforeBlobId
    , pcfccrsLocation
    , pcfccrsAfterCommitId
    , pcfccrsAfterBlobId
    , pcfccrsBeforeCommitId
    , pcfccrsRepositoryName
    , pcfccrsComment
    , pcfccrsResponseStatus
    ) where

import Network.AWS.CodeCommit.Types
import Network.AWS.CodeCommit.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'postCommentForComparedCommit' smart constructor.
data PostCommentForComparedCommit = PostCommentForComparedCommit'
  { _pcfccLocation           :: !(Maybe Location)
  , _pcfccBeforeCommitId     :: !(Maybe Text)
  , _pcfccClientRequestToken :: !(Maybe Text)
  , _pcfccRepositoryName     :: !Text
  , _pcfccAfterCommitId      :: !Text
  , _pcfccContent            :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PostCommentForComparedCommit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcfccLocation' - The location of the comparison where you want to comment.
--
-- * 'pcfccBeforeCommitId' - To establish the directionality of the comparison, the full commit ID of the 'before' commit.
--
-- * 'pcfccClientRequestToken' - A unique, client-generated idempotency token that when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request will return information about the initial request that used that token.
--
-- * 'pcfccRepositoryName' - The name of the repository where you want to post a comment on the comparison between commits.
--
-- * 'pcfccAfterCommitId' - To establish the directionality of the comparison, the full commit ID of the 'after' commit.
--
-- * 'pcfccContent' - The content of the comment you want to make.
postCommentForComparedCommit
    :: Text -- ^ 'pcfccRepositoryName'
    -> Text -- ^ 'pcfccAfterCommitId'
    -> Text -- ^ 'pcfccContent'
    -> PostCommentForComparedCommit
postCommentForComparedCommit pRepositoryName_ pAfterCommitId_ pContent_ =
  PostCommentForComparedCommit'
    { _pcfccLocation = Nothing
    , _pcfccBeforeCommitId = Nothing
    , _pcfccClientRequestToken = Nothing
    , _pcfccRepositoryName = pRepositoryName_
    , _pcfccAfterCommitId = pAfterCommitId_
    , _pcfccContent = pContent_
    }


-- | The location of the comparison where you want to comment.
pcfccLocation :: Lens' PostCommentForComparedCommit (Maybe Location)
pcfccLocation = lens _pcfccLocation (\ s a -> s{_pcfccLocation = a})

-- | To establish the directionality of the comparison, the full commit ID of the 'before' commit.
pcfccBeforeCommitId :: Lens' PostCommentForComparedCommit (Maybe Text)
pcfccBeforeCommitId = lens _pcfccBeforeCommitId (\ s a -> s{_pcfccBeforeCommitId = a})

-- | A unique, client-generated idempotency token that when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request will return information about the initial request that used that token.
pcfccClientRequestToken :: Lens' PostCommentForComparedCommit (Maybe Text)
pcfccClientRequestToken = lens _pcfccClientRequestToken (\ s a -> s{_pcfccClientRequestToken = a})

-- | The name of the repository where you want to post a comment on the comparison between commits.
pcfccRepositoryName :: Lens' PostCommentForComparedCommit Text
pcfccRepositoryName = lens _pcfccRepositoryName (\ s a -> s{_pcfccRepositoryName = a})

-- | To establish the directionality of the comparison, the full commit ID of the 'after' commit.
pcfccAfterCommitId :: Lens' PostCommentForComparedCommit Text
pcfccAfterCommitId = lens _pcfccAfterCommitId (\ s a -> s{_pcfccAfterCommitId = a})

-- | The content of the comment you want to make.
pcfccContent :: Lens' PostCommentForComparedCommit Text
pcfccContent = lens _pcfccContent (\ s a -> s{_pcfccContent = a})

instance AWSRequest PostCommentForComparedCommit
         where
        type Rs PostCommentForComparedCommit =
             PostCommentForComparedCommitResponse
        request = postJSON codeCommit
        response
          = receiveJSON
              (\ s h x ->
                 PostCommentForComparedCommitResponse' <$>
                   (x .?> "beforeBlobId") <*> (x .?> "location") <*>
                     (x .?> "afterCommitId")
                     <*> (x .?> "afterBlobId")
                     <*> (x .?> "beforeCommitId")
                     <*> (x .?> "repositoryName")
                     <*> (x .?> "comment")
                     <*> (pure (fromEnum s)))

instance Hashable PostCommentForComparedCommit where

instance NFData PostCommentForComparedCommit where

instance ToHeaders PostCommentForComparedCommit where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.PostCommentForComparedCommit"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PostCommentForComparedCommit where
        toJSON PostCommentForComparedCommit'{..}
          = object
              (catMaybes
                 [("location" .=) <$> _pcfccLocation,
                  ("beforeCommitId" .=) <$> _pcfccBeforeCommitId,
                  ("clientRequestToken" .=) <$>
                    _pcfccClientRequestToken,
                  Just ("repositoryName" .= _pcfccRepositoryName),
                  Just ("afterCommitId" .= _pcfccAfterCommitId),
                  Just ("content" .= _pcfccContent)])

instance ToPath PostCommentForComparedCommit where
        toPath = const "/"

instance ToQuery PostCommentForComparedCommit where
        toQuery = const mempty

-- | /See:/ 'postCommentForComparedCommitResponse' smart constructor.
data PostCommentForComparedCommitResponse = PostCommentForComparedCommitResponse'
  { _pcfccrsBeforeBlobId   :: !(Maybe Text)
  , _pcfccrsLocation       :: !(Maybe Location)
  , _pcfccrsAfterCommitId  :: !(Maybe Text)
  , _pcfccrsAfterBlobId    :: !(Maybe Text)
  , _pcfccrsBeforeCommitId :: !(Maybe Text)
  , _pcfccrsRepositoryName :: !(Maybe Text)
  , _pcfccrsComment        :: !(Maybe Comment)
  , _pcfccrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PostCommentForComparedCommitResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcfccrsBeforeBlobId' - In the directionality you established, the blob ID of the 'before' blob.
--
-- * 'pcfccrsLocation' - The location of the comment in the comparison between the two commits.
--
-- * 'pcfccrsAfterCommitId' - In the directionality you established, the full commit ID of the 'after' commit.
--
-- * 'pcfccrsAfterBlobId' - In the directionality you established, the blob ID of the 'after' blob.
--
-- * 'pcfccrsBeforeCommitId' - In the directionality you established, the full commit ID of the 'before' commit.
--
-- * 'pcfccrsRepositoryName' - The name of the repository where you posted a comment on the comparison between commits.
--
-- * 'pcfccrsComment' - The content of the comment you posted.
--
-- * 'pcfccrsResponseStatus' - -- | The response status code.
postCommentForComparedCommitResponse
    :: Int -- ^ 'pcfccrsResponseStatus'
    -> PostCommentForComparedCommitResponse
postCommentForComparedCommitResponse pResponseStatus_ =
  PostCommentForComparedCommitResponse'
    { _pcfccrsBeforeBlobId = Nothing
    , _pcfccrsLocation = Nothing
    , _pcfccrsAfterCommitId = Nothing
    , _pcfccrsAfterBlobId = Nothing
    , _pcfccrsBeforeCommitId = Nothing
    , _pcfccrsRepositoryName = Nothing
    , _pcfccrsComment = Nothing
    , _pcfccrsResponseStatus = pResponseStatus_
    }


-- | In the directionality you established, the blob ID of the 'before' blob.
pcfccrsBeforeBlobId :: Lens' PostCommentForComparedCommitResponse (Maybe Text)
pcfccrsBeforeBlobId = lens _pcfccrsBeforeBlobId (\ s a -> s{_pcfccrsBeforeBlobId = a})

-- | The location of the comment in the comparison between the two commits.
pcfccrsLocation :: Lens' PostCommentForComparedCommitResponse (Maybe Location)
pcfccrsLocation = lens _pcfccrsLocation (\ s a -> s{_pcfccrsLocation = a})

-- | In the directionality you established, the full commit ID of the 'after' commit.
pcfccrsAfterCommitId :: Lens' PostCommentForComparedCommitResponse (Maybe Text)
pcfccrsAfterCommitId = lens _pcfccrsAfterCommitId (\ s a -> s{_pcfccrsAfterCommitId = a})

-- | In the directionality you established, the blob ID of the 'after' blob.
pcfccrsAfterBlobId :: Lens' PostCommentForComparedCommitResponse (Maybe Text)
pcfccrsAfterBlobId = lens _pcfccrsAfterBlobId (\ s a -> s{_pcfccrsAfterBlobId = a})

-- | In the directionality you established, the full commit ID of the 'before' commit.
pcfccrsBeforeCommitId :: Lens' PostCommentForComparedCommitResponse (Maybe Text)
pcfccrsBeforeCommitId = lens _pcfccrsBeforeCommitId (\ s a -> s{_pcfccrsBeforeCommitId = a})

-- | The name of the repository where you posted a comment on the comparison between commits.
pcfccrsRepositoryName :: Lens' PostCommentForComparedCommitResponse (Maybe Text)
pcfccrsRepositoryName = lens _pcfccrsRepositoryName (\ s a -> s{_pcfccrsRepositoryName = a})

-- | The content of the comment you posted.
pcfccrsComment :: Lens' PostCommentForComparedCommitResponse (Maybe Comment)
pcfccrsComment = lens _pcfccrsComment (\ s a -> s{_pcfccrsComment = a})

-- | -- | The response status code.
pcfccrsResponseStatus :: Lens' PostCommentForComparedCommitResponse Int
pcfccrsResponseStatus = lens _pcfccrsResponseStatus (\ s a -> s{_pcfccrsResponseStatus = a})

instance NFData PostCommentForComparedCommitResponse
         where
