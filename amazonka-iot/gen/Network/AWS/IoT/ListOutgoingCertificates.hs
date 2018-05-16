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
-- Module      : Network.AWS.IoT.ListOutgoingCertificates
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists certificates that are being transferred but not yet accepted.
--
--
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListOutgoingCertificates
    (
    -- * Creating a Request
      listOutgoingCertificates
    , ListOutgoingCertificates
    -- * Request Lenses
    , locMarker
    , locAscendingOrder
    , locPageSize

    -- * Destructuring the Response
    , listOutgoingCertificatesResponse
    , ListOutgoingCertificatesResponse
    -- * Response Lenses
    , locrsNextMarker
    , locrsOutgoingCertificates
    , locrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input to the ListOutgoingCertificates operation.
--
--
--
-- /See:/ 'listOutgoingCertificates' smart constructor.
data ListOutgoingCertificates = ListOutgoingCertificates'
  { _locMarker         :: !(Maybe Text)
  , _locAscendingOrder :: !(Maybe Bool)
  , _locPageSize       :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListOutgoingCertificates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'locMarker' - The marker for the next set of results.
--
-- * 'locAscendingOrder' - Specifies the order for results. If True, the results are returned in ascending order, based on the creation date.
--
-- * 'locPageSize' - The result page size.
listOutgoingCertificates
    :: ListOutgoingCertificates
listOutgoingCertificates =
  ListOutgoingCertificates'
    {_locMarker = Nothing, _locAscendingOrder = Nothing, _locPageSize = Nothing}


-- | The marker for the next set of results.
locMarker :: Lens' ListOutgoingCertificates (Maybe Text)
locMarker = lens _locMarker (\ s a -> s{_locMarker = a})

-- | Specifies the order for results. If True, the results are returned in ascending order, based on the creation date.
locAscendingOrder :: Lens' ListOutgoingCertificates (Maybe Bool)
locAscendingOrder = lens _locAscendingOrder (\ s a -> s{_locAscendingOrder = a})

-- | The result page size.
locPageSize :: Lens' ListOutgoingCertificates (Maybe Natural)
locPageSize = lens _locPageSize (\ s a -> s{_locPageSize = a}) . mapping _Nat

instance AWSPager ListOutgoingCertificates where
        page rq rs
          | stop (rs ^. locrsNextMarker) = Nothing
          | stop (rs ^. locrsOutgoingCertificates) = Nothing
          | otherwise =
            Just $ rq & locMarker .~ rs ^. locrsNextMarker

instance AWSRequest ListOutgoingCertificates where
        type Rs ListOutgoingCertificates =
             ListOutgoingCertificatesResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 ListOutgoingCertificatesResponse' <$>
                   (x .?> "nextMarker") <*>
                     (x .?> "outgoingCertificates" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListOutgoingCertificates where

instance NFData ListOutgoingCertificates where

instance ToHeaders ListOutgoingCertificates where
        toHeaders = const mempty

instance ToPath ListOutgoingCertificates where
        toPath = const "/certificates-out-going"

instance ToQuery ListOutgoingCertificates where
        toQuery ListOutgoingCertificates'{..}
          = mconcat
              ["marker" =: _locMarker,
               "isAscendingOrder" =: _locAscendingOrder,
               "pageSize" =: _locPageSize]

-- | The output from the ListOutgoingCertificates operation.
--
--
--
-- /See:/ 'listOutgoingCertificatesResponse' smart constructor.
data ListOutgoingCertificatesResponse = ListOutgoingCertificatesResponse'
  { _locrsNextMarker           :: !(Maybe Text)
  , _locrsOutgoingCertificates :: !(Maybe [OutgoingCertificate])
  , _locrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListOutgoingCertificatesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'locrsNextMarker' - The marker for the next set of results.
--
-- * 'locrsOutgoingCertificates' - The certificates that are being transferred but not yet accepted.
--
-- * 'locrsResponseStatus' - -- | The response status code.
listOutgoingCertificatesResponse
    :: Int -- ^ 'locrsResponseStatus'
    -> ListOutgoingCertificatesResponse
listOutgoingCertificatesResponse pResponseStatus_ =
  ListOutgoingCertificatesResponse'
    { _locrsNextMarker = Nothing
    , _locrsOutgoingCertificates = Nothing
    , _locrsResponseStatus = pResponseStatus_
    }


-- | The marker for the next set of results.
locrsNextMarker :: Lens' ListOutgoingCertificatesResponse (Maybe Text)
locrsNextMarker = lens _locrsNextMarker (\ s a -> s{_locrsNextMarker = a})

-- | The certificates that are being transferred but not yet accepted.
locrsOutgoingCertificates :: Lens' ListOutgoingCertificatesResponse [OutgoingCertificate]
locrsOutgoingCertificates = lens _locrsOutgoingCertificates (\ s a -> s{_locrsOutgoingCertificates = a}) . _Default . _Coerce

-- | -- | The response status code.
locrsResponseStatus :: Lens' ListOutgoingCertificatesResponse Int
locrsResponseStatus = lens _locrsResponseStatus (\ s a -> s{_locrsResponseStatus = a})

instance NFData ListOutgoingCertificatesResponse
         where
