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
-- Module      : Network.AWS.IoT.ListCertificates
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the certificates registered in your AWS account.
--
--
-- The results are paginated with a default page size of 25. You can use the returned marker to retrieve additional results.
--
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListCertificates
    (
    -- * Creating a Request
      listCertificates
    , ListCertificates
    -- * Request Lenses
    , lcMarker
    , lcAscendingOrder
    , lcPageSize

    -- * Destructuring the Response
    , listCertificatesResponse
    , ListCertificatesResponse
    -- * Response Lenses
    , lcrsCertificates
    , lcrsNextMarker
    , lcrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the ListCertificates operation.
--
--
--
-- /See:/ 'listCertificates' smart constructor.
data ListCertificates = ListCertificates'
  { _lcMarker         :: !(Maybe Text)
  , _lcAscendingOrder :: !(Maybe Bool)
  , _lcPageSize       :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListCertificates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcMarker' - The marker for the next set of results.
--
-- * 'lcAscendingOrder' - Specifies the order for results. If True, the results are returned in ascending order, based on the creation date.
--
-- * 'lcPageSize' - The result page size.
listCertificates
    :: ListCertificates
listCertificates =
  ListCertificates'
    {_lcMarker = Nothing, _lcAscendingOrder = Nothing, _lcPageSize = Nothing}


-- | The marker for the next set of results.
lcMarker :: Lens' ListCertificates (Maybe Text)
lcMarker = lens _lcMarker (\ s a -> s{_lcMarker = a})

-- | Specifies the order for results. If True, the results are returned in ascending order, based on the creation date.
lcAscendingOrder :: Lens' ListCertificates (Maybe Bool)
lcAscendingOrder = lens _lcAscendingOrder (\ s a -> s{_lcAscendingOrder = a})

-- | The result page size.
lcPageSize :: Lens' ListCertificates (Maybe Natural)
lcPageSize = lens _lcPageSize (\ s a -> s{_lcPageSize = a}) . mapping _Nat

instance AWSPager ListCertificates where
        page rq rs
          | stop (rs ^. lcrsNextMarker) = Nothing
          | stop (rs ^. lcrsCertificates) = Nothing
          | otherwise =
            Just $ rq & lcMarker .~ rs ^. lcrsNextMarker

instance AWSRequest ListCertificates where
        type Rs ListCertificates = ListCertificatesResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 ListCertificatesResponse' <$>
                   (x .?> "certificates" .!@ mempty) <*>
                     (x .?> "nextMarker")
                     <*> (pure (fromEnum s)))

instance Hashable ListCertificates where

instance NFData ListCertificates where

instance ToHeaders ListCertificates where
        toHeaders = const mempty

instance ToPath ListCertificates where
        toPath = const "/certificates"

instance ToQuery ListCertificates where
        toQuery ListCertificates'{..}
          = mconcat
              ["marker" =: _lcMarker,
               "isAscendingOrder" =: _lcAscendingOrder,
               "pageSize" =: _lcPageSize]

-- | The output of the ListCertificates operation.
--
--
--
-- /See:/ 'listCertificatesResponse' smart constructor.
data ListCertificatesResponse = ListCertificatesResponse'
  { _lcrsCertificates   :: !(Maybe [Certificate])
  , _lcrsNextMarker     :: !(Maybe Text)
  , _lcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListCertificatesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcrsCertificates' - The descriptions of the certificates.
--
-- * 'lcrsNextMarker' - The marker for the next set of results, or null if there are no additional results.
--
-- * 'lcrsResponseStatus' - -- | The response status code.
listCertificatesResponse
    :: Int -- ^ 'lcrsResponseStatus'
    -> ListCertificatesResponse
listCertificatesResponse pResponseStatus_ =
  ListCertificatesResponse'
    { _lcrsCertificates = Nothing
    , _lcrsNextMarker = Nothing
    , _lcrsResponseStatus = pResponseStatus_
    }


-- | The descriptions of the certificates.
lcrsCertificates :: Lens' ListCertificatesResponse [Certificate]
lcrsCertificates = lens _lcrsCertificates (\ s a -> s{_lcrsCertificates = a}) . _Default . _Coerce

-- | The marker for the next set of results, or null if there are no additional results.
lcrsNextMarker :: Lens' ListCertificatesResponse (Maybe Text)
lcrsNextMarker = lens _lcrsNextMarker (\ s a -> s{_lcrsNextMarker = a})

-- | -- | The response status code.
lcrsResponseStatus :: Lens' ListCertificatesResponse Int
lcrsResponseStatus = lens _lcrsResponseStatus (\ s a -> s{_lcrsResponseStatus = a})

instance NFData ListCertificatesResponse where
