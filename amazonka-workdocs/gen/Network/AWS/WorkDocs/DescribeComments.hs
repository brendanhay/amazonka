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
-- Module      : Network.AWS.WorkDocs.DescribeComments
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all the comments for the specified document version.
--
--
module Network.AWS.WorkDocs.DescribeComments
    (
    -- * Creating a Request
      describeComments
    , DescribeComments
    -- * Request Lenses
    , dcAuthenticationToken
    , dcMarker
    , dcLimit
    , dcDocumentId
    , dcVersionId

    -- * Destructuring the Response
    , describeCommentsResponse
    , DescribeCommentsResponse
    -- * Response Lenses
    , dcrsMarker
    , dcrsComments
    , dcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkDocs.Types
import Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'describeComments' smart constructor.
data DescribeComments = DescribeComments'
  { _dcAuthenticationToken :: !(Maybe (Sensitive Text))
  , _dcMarker              :: !(Maybe Text)
  , _dcLimit               :: !(Maybe Nat)
  , _dcDocumentId          :: !Text
  , _dcVersionId           :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeComments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcAuthenticationToken' - Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
--
-- * 'dcMarker' - The marker for the next set of results. This marker was received from a previous call.
--
-- * 'dcLimit' - The maximum number of items to return.
--
-- * 'dcDocumentId' - The ID of the document.
--
-- * 'dcVersionId' - The ID of the document version.
describeComments
    :: Text -- ^ 'dcDocumentId'
    -> Text -- ^ 'dcVersionId'
    -> DescribeComments
describeComments pDocumentId_ pVersionId_ =
  DescribeComments'
    { _dcAuthenticationToken = Nothing
    , _dcMarker = Nothing
    , _dcLimit = Nothing
    , _dcDocumentId = pDocumentId_
    , _dcVersionId = pVersionId_
    }


-- | Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
dcAuthenticationToken :: Lens' DescribeComments (Maybe Text)
dcAuthenticationToken = lens _dcAuthenticationToken (\ s a -> s{_dcAuthenticationToken = a}) . mapping _Sensitive

-- | The marker for the next set of results. This marker was received from a previous call.
dcMarker :: Lens' DescribeComments (Maybe Text)
dcMarker = lens _dcMarker (\ s a -> s{_dcMarker = a})

-- | The maximum number of items to return.
dcLimit :: Lens' DescribeComments (Maybe Natural)
dcLimit = lens _dcLimit (\ s a -> s{_dcLimit = a}) . mapping _Nat

-- | The ID of the document.
dcDocumentId :: Lens' DescribeComments Text
dcDocumentId = lens _dcDocumentId (\ s a -> s{_dcDocumentId = a})

-- | The ID of the document version.
dcVersionId :: Lens' DescribeComments Text
dcVersionId = lens _dcVersionId (\ s a -> s{_dcVersionId = a})

instance AWSRequest DescribeComments where
        type Rs DescribeComments = DescribeCommentsResponse
        request = get workDocs
        response
          = receiveJSON
              (\ s h x ->
                 DescribeCommentsResponse' <$>
                   (x .?> "Marker") <*> (x .?> "Comments" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeComments where

instance NFData DescribeComments where

instance ToHeaders DescribeComments where
        toHeaders DescribeComments'{..}
          = mconcat
              ["Authentication" =# _dcAuthenticationToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToPath DescribeComments where
        toPath DescribeComments'{..}
          = mconcat
              ["/api/v1/documents/", toBS _dcDocumentId,
               "/versions/", toBS _dcVersionId, "/comments"]

instance ToQuery DescribeComments where
        toQuery DescribeComments'{..}
          = mconcat
              ["marker" =: _dcMarker, "limit" =: _dcLimit]

-- | /See:/ 'describeCommentsResponse' smart constructor.
data DescribeCommentsResponse = DescribeCommentsResponse'
  { _dcrsMarker         :: !(Maybe Text)
  , _dcrsComments       :: !(Maybe [Comment])
  , _dcrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeCommentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrsMarker' - The marker for the next set of results. This marker was received from a previous call.
--
-- * 'dcrsComments' - The list of comments for the specified document version.
--
-- * 'dcrsResponseStatus' - -- | The response status code.
describeCommentsResponse
    :: Int -- ^ 'dcrsResponseStatus'
    -> DescribeCommentsResponse
describeCommentsResponse pResponseStatus_ =
  DescribeCommentsResponse'
    { _dcrsMarker = Nothing
    , _dcrsComments = Nothing
    , _dcrsResponseStatus = pResponseStatus_
    }


-- | The marker for the next set of results. This marker was received from a previous call.
dcrsMarker :: Lens' DescribeCommentsResponse (Maybe Text)
dcrsMarker = lens _dcrsMarker (\ s a -> s{_dcrsMarker = a})

-- | The list of comments for the specified document version.
dcrsComments :: Lens' DescribeCommentsResponse [Comment]
dcrsComments = lens _dcrsComments (\ s a -> s{_dcrsComments = a}) . _Default . _Coerce

-- | -- | The response status code.
dcrsResponseStatus :: Lens' DescribeCommentsResponse Int
dcrsResponseStatus = lens _dcrsResponseStatus (\ s a -> s{_dcrsResponseStatus = a})

instance NFData DescribeCommentsResponse where
