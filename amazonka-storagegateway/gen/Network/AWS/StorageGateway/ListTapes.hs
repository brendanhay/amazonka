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
-- Module      : Network.AWS.StorageGateway.ListTapes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists virtual tapes in your virtual tape library (VTL) and your virtual tape shelf (VTS). You specify the tapes to list by specifying one or more tape Amazon Resource Names (ARNs). If you don't specify a tape ARN, the operation lists all virtual tapes in both your VTL and VTS.
--
--
-- This operation supports pagination. By default, the operation returns a maximum of up to 100 tapes. You can optionally specify the @Limit@ parameter in the body to limit the number of tapes in the response. If the number of tapes returned in the response is truncated, the response includes a @Marker@ element that you can use in your subsequent request to retrieve the next set of tapes. This operation is only supported in the tape gateway type.
--
module Network.AWS.StorageGateway.ListTapes
    (
    -- * Creating a Request
      listTapes
    , ListTapes
    -- * Request Lenses
    , ltMarker
    , ltLimit
    , ltTapeARNs

    -- * Destructuring the Response
    , listTapesResponse
    , ListTapesResponse
    -- * Response Lenses
    , ltrsMarker
    , ltrsTapeInfos
    , ltrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | A JSON object that contains one or more of the following fields:
--
--
--     * 'ListTapesInput$Limit'
--
--     * 'ListTapesInput$Marker'
--
--     * 'ListTapesInput$TapeARNs'
--
--
--
--
-- /See:/ 'listTapes' smart constructor.
data ListTapes = ListTapes'
  { _ltMarker   :: !(Maybe Text)
  , _ltLimit    :: !(Maybe Nat)
  , _ltTapeARNs :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTapes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltMarker' - A string that indicates the position at which to begin the returned list of tapes.
--
-- * 'ltLimit' - An optional number limit for the tapes in the list returned by this call.
--
-- * 'ltTapeARNs' - Undocumented member.
listTapes
    :: ListTapes
listTapes =
  ListTapes' {_ltMarker = Nothing, _ltLimit = Nothing, _ltTapeARNs = Nothing}


-- | A string that indicates the position at which to begin the returned list of tapes.
ltMarker :: Lens' ListTapes (Maybe Text)
ltMarker = lens _ltMarker (\ s a -> s{_ltMarker = a})

-- | An optional number limit for the tapes in the list returned by this call.
ltLimit :: Lens' ListTapes (Maybe Natural)
ltLimit = lens _ltLimit (\ s a -> s{_ltLimit = a}) . mapping _Nat

-- | Undocumented member.
ltTapeARNs :: Lens' ListTapes [Text]
ltTapeARNs = lens _ltTapeARNs (\ s a -> s{_ltTapeARNs = a}) . _Default . _Coerce

instance AWSRequest ListTapes where
        type Rs ListTapes = ListTapesResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 ListTapesResponse' <$>
                   (x .?> "Marker") <*> (x .?> "TapeInfos" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListTapes where

instance NFData ListTapes where

instance ToHeaders ListTapes where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.ListTapes" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListTapes where
        toJSON ListTapes'{..}
          = object
              (catMaybes
                 [("Marker" .=) <$> _ltMarker,
                  ("Limit" .=) <$> _ltLimit,
                  ("TapeARNs" .=) <$> _ltTapeARNs])

instance ToPath ListTapes where
        toPath = const "/"

instance ToQuery ListTapes where
        toQuery = const mempty

-- | A JSON object containing the following fields:
--
--
--     * 'ListTapesOutput$Marker'
--
--     * 'ListTapesOutput$VolumeInfos'
--
--
--
--
-- /See:/ 'listTapesResponse' smart constructor.
data ListTapesResponse = ListTapesResponse'
  { _ltrsMarker         :: !(Maybe Text)
  , _ltrsTapeInfos      :: !(Maybe [TapeInfo])
  , _ltrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTapesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltrsMarker' - A string that indicates the position at which to begin returning the next list of tapes. Use the marker in your next request to continue pagination of tapes. If there are no more tapes to list, this element does not appear in the response body.
--
-- * 'ltrsTapeInfos' - Undocumented member.
--
-- * 'ltrsResponseStatus' - -- | The response status code.
listTapesResponse
    :: Int -- ^ 'ltrsResponseStatus'
    -> ListTapesResponse
listTapesResponse pResponseStatus_ =
  ListTapesResponse'
    { _ltrsMarker = Nothing
    , _ltrsTapeInfos = Nothing
    , _ltrsResponseStatus = pResponseStatus_
    }


-- | A string that indicates the position at which to begin returning the next list of tapes. Use the marker in your next request to continue pagination of tapes. If there are no more tapes to list, this element does not appear in the response body.
ltrsMarker :: Lens' ListTapesResponse (Maybe Text)
ltrsMarker = lens _ltrsMarker (\ s a -> s{_ltrsMarker = a})

-- | Undocumented member.
ltrsTapeInfos :: Lens' ListTapesResponse [TapeInfo]
ltrsTapeInfos = lens _ltrsTapeInfos (\ s a -> s{_ltrsTapeInfos = a}) . _Default . _Coerce

-- | -- | The response status code.
ltrsResponseStatus :: Lens' ListTapesResponse Int
ltrsResponseStatus = lens _ltrsResponseStatus (\ s a -> s{_ltrsResponseStatus = a})

instance NFData ListTapesResponse where
