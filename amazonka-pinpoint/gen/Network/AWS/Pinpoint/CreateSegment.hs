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
-- Module      : Network.AWS.Pinpoint.CreateSegment
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to create or update a segment.
module Network.AWS.Pinpoint.CreateSegment
    (
    -- * Creating a Request
      createSegment
    , CreateSegment
    -- * Request Lenses
    , csApplicationId
    , csWriteSegmentRequest

    -- * Destructuring the Response
    , createSegmentResponse
    , CreateSegmentResponse
    -- * Response Lenses
    , csrsResponseStatus
    , csrsSegmentResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createSegment' smart constructor.
data CreateSegment = CreateSegment'
  { _csApplicationId       :: !Text
  , _csWriteSegmentRequest :: !WriteSegmentRequest
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateSegment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csApplicationId' - Undocumented member.
--
-- * 'csWriteSegmentRequest' - Undocumented member.
createSegment
    :: Text -- ^ 'csApplicationId'
    -> WriteSegmentRequest -- ^ 'csWriteSegmentRequest'
    -> CreateSegment
createSegment pApplicationId_ pWriteSegmentRequest_ =
  CreateSegment'
    { _csApplicationId = pApplicationId_
    , _csWriteSegmentRequest = pWriteSegmentRequest_
    }


-- | Undocumented member.
csApplicationId :: Lens' CreateSegment Text
csApplicationId = lens _csApplicationId (\ s a -> s{_csApplicationId = a})

-- | Undocumented member.
csWriteSegmentRequest :: Lens' CreateSegment WriteSegmentRequest
csWriteSegmentRequest = lens _csWriteSegmentRequest (\ s a -> s{_csWriteSegmentRequest = a})

instance AWSRequest CreateSegment where
        type Rs CreateSegment = CreateSegmentResponse
        request = postJSON pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 CreateSegmentResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable CreateSegment where

instance NFData CreateSegment where

instance ToHeaders CreateSegment where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateSegment where
        toJSON CreateSegment'{..}
          = object
              (catMaybes
                 [Just
                    ("WriteSegmentRequest" .= _csWriteSegmentRequest)])

instance ToPath CreateSegment where
        toPath CreateSegment'{..}
          = mconcat
              ["/v1/apps/", toBS _csApplicationId, "/segments"]

instance ToQuery CreateSegment where
        toQuery = const mempty

-- | /See:/ 'createSegmentResponse' smart constructor.
data CreateSegmentResponse = CreateSegmentResponse'
  { _csrsResponseStatus  :: !Int
  , _csrsSegmentResponse :: !SegmentResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateSegmentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csrsResponseStatus' - -- | The response status code.
--
-- * 'csrsSegmentResponse' - Undocumented member.
createSegmentResponse
    :: Int -- ^ 'csrsResponseStatus'
    -> SegmentResponse -- ^ 'csrsSegmentResponse'
    -> CreateSegmentResponse
createSegmentResponse pResponseStatus_ pSegmentResponse_ =
  CreateSegmentResponse'
    { _csrsResponseStatus = pResponseStatus_
    , _csrsSegmentResponse = pSegmentResponse_
    }


-- | -- | The response status code.
csrsResponseStatus :: Lens' CreateSegmentResponse Int
csrsResponseStatus = lens _csrsResponseStatus (\ s a -> s{_csrsResponseStatus = a})

-- | Undocumented member.
csrsSegmentResponse :: Lens' CreateSegmentResponse SegmentResponse
csrsSegmentResponse = lens _csrsSegmentResponse (\ s a -> s{_csrsSegmentResponse = a})

instance NFData CreateSegmentResponse where
