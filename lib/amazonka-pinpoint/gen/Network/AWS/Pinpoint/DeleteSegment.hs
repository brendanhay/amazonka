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
-- Module      : Network.AWS.Pinpoint.DeleteSegment
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a segment.
module Network.AWS.Pinpoint.DeleteSegment
    (
    -- * Creating a Request
      deleteSegment
    , DeleteSegment
    -- * Request Lenses
    , dsSegmentId
    , dsApplicationId

    -- * Destructuring the Response
    , deleteSegmentResponse
    , DeleteSegmentResponse
    -- * Response Lenses
    , dsrsResponseStatus
    , dsrsSegmentResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteSegment' smart constructor.
data DeleteSegment = DeleteSegment'
  { _dsSegmentId     :: !Text
  , _dsApplicationId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSegment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsSegmentId' - Undocumented member.
--
-- * 'dsApplicationId' - Undocumented member.
deleteSegment
    :: Text -- ^ 'dsSegmentId'
    -> Text -- ^ 'dsApplicationId'
    -> DeleteSegment
deleteSegment pSegmentId_ pApplicationId_ =
  DeleteSegment'
    {_dsSegmentId = pSegmentId_, _dsApplicationId = pApplicationId_}


-- | Undocumented member.
dsSegmentId :: Lens' DeleteSegment Text
dsSegmentId = lens _dsSegmentId (\ s a -> s{_dsSegmentId = a})

-- | Undocumented member.
dsApplicationId :: Lens' DeleteSegment Text
dsApplicationId = lens _dsApplicationId (\ s a -> s{_dsApplicationId = a})

instance AWSRequest DeleteSegment where
        type Rs DeleteSegment = DeleteSegmentResponse
        request = delete pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 DeleteSegmentResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable DeleteSegment where

instance NFData DeleteSegment where

instance ToHeaders DeleteSegment where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteSegment where
        toPath DeleteSegment'{..}
          = mconcat
              ["/v1/apps/", toBS _dsApplicationId, "/segments/",
               toBS _dsSegmentId]

instance ToQuery DeleteSegment where
        toQuery = const mempty

-- | /See:/ 'deleteSegmentResponse' smart constructor.
data DeleteSegmentResponse = DeleteSegmentResponse'
  { _dsrsResponseStatus  :: !Int
  , _dsrsSegmentResponse :: !SegmentResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSegmentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsrsResponseStatus' - -- | The response status code.
--
-- * 'dsrsSegmentResponse' - Undocumented member.
deleteSegmentResponse
    :: Int -- ^ 'dsrsResponseStatus'
    -> SegmentResponse -- ^ 'dsrsSegmentResponse'
    -> DeleteSegmentResponse
deleteSegmentResponse pResponseStatus_ pSegmentResponse_ =
  DeleteSegmentResponse'
    { _dsrsResponseStatus = pResponseStatus_
    , _dsrsSegmentResponse = pSegmentResponse_
    }


-- | -- | The response status code.
dsrsResponseStatus :: Lens' DeleteSegmentResponse Int
dsrsResponseStatus = lens _dsrsResponseStatus (\ s a -> s{_dsrsResponseStatus = a})

-- | Undocumented member.
dsrsSegmentResponse :: Lens' DeleteSegmentResponse SegmentResponse
dsrsSegmentResponse = lens _dsrsSegmentResponse (\ s a -> s{_dsrsSegmentResponse = a})

instance NFData DeleteSegmentResponse where
