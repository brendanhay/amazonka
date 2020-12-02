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
-- Module      : Network.AWS.IoT.ListCertificatesByCA
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the device certificates signed by the specified CA certificate.
--
--
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListCertificatesByCA
    (
    -- * Creating a Request
      listCertificatesByCA
    , ListCertificatesByCA
    -- * Request Lenses
    , lcbcaMarker
    , lcbcaAscendingOrder
    , lcbcaPageSize
    , lcbcaCaCertificateId

    -- * Destructuring the Response
    , listCertificatesByCAResponse
    , ListCertificatesByCAResponse
    -- * Response Lenses
    , lcbcarsCertificates
    , lcbcarsNextMarker
    , lcbcarsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input to the ListCertificatesByCA operation.
--
--
--
-- /See:/ 'listCertificatesByCA' smart constructor.
data ListCertificatesByCA = ListCertificatesByCA'
  { _lcbcaMarker          :: !(Maybe Text)
  , _lcbcaAscendingOrder  :: !(Maybe Bool)
  , _lcbcaPageSize        :: !(Maybe Nat)
  , _lcbcaCaCertificateId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListCertificatesByCA' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcbcaMarker' - The marker for the next set of results.
--
-- * 'lcbcaAscendingOrder' - Specifies the order for results. If True, the results are returned in ascending order, based on the creation date.
--
-- * 'lcbcaPageSize' - The result page size.
--
-- * 'lcbcaCaCertificateId' - The ID of the CA certificate. This operation will list all registered device certificate that were signed by this CA certificate.
listCertificatesByCA
    :: Text -- ^ 'lcbcaCaCertificateId'
    -> ListCertificatesByCA
listCertificatesByCA pCaCertificateId_ =
  ListCertificatesByCA'
    { _lcbcaMarker = Nothing
    , _lcbcaAscendingOrder = Nothing
    , _lcbcaPageSize = Nothing
    , _lcbcaCaCertificateId = pCaCertificateId_
    }


-- | The marker for the next set of results.
lcbcaMarker :: Lens' ListCertificatesByCA (Maybe Text)
lcbcaMarker = lens _lcbcaMarker (\ s a -> s{_lcbcaMarker = a})

-- | Specifies the order for results. If True, the results are returned in ascending order, based on the creation date.
lcbcaAscendingOrder :: Lens' ListCertificatesByCA (Maybe Bool)
lcbcaAscendingOrder = lens _lcbcaAscendingOrder (\ s a -> s{_lcbcaAscendingOrder = a})

-- | The result page size.
lcbcaPageSize :: Lens' ListCertificatesByCA (Maybe Natural)
lcbcaPageSize = lens _lcbcaPageSize (\ s a -> s{_lcbcaPageSize = a}) . mapping _Nat

-- | The ID of the CA certificate. This operation will list all registered device certificate that were signed by this CA certificate.
lcbcaCaCertificateId :: Lens' ListCertificatesByCA Text
lcbcaCaCertificateId = lens _lcbcaCaCertificateId (\ s a -> s{_lcbcaCaCertificateId = a})

instance AWSPager ListCertificatesByCA where
        page rq rs
          | stop (rs ^. lcbcarsNextMarker) = Nothing
          | stop (rs ^. lcbcarsCertificates) = Nothing
          | otherwise =
            Just $ rq & lcbcaMarker .~ rs ^. lcbcarsNextMarker

instance AWSRequest ListCertificatesByCA where
        type Rs ListCertificatesByCA =
             ListCertificatesByCAResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 ListCertificatesByCAResponse' <$>
                   (x .?> "certificates" .!@ mempty) <*>
                     (x .?> "nextMarker")
                     <*> (pure (fromEnum s)))

instance Hashable ListCertificatesByCA where

instance NFData ListCertificatesByCA where

instance ToHeaders ListCertificatesByCA where
        toHeaders = const mempty

instance ToPath ListCertificatesByCA where
        toPath ListCertificatesByCA'{..}
          = mconcat
              ["/certificates-by-ca/", toBS _lcbcaCaCertificateId]

instance ToQuery ListCertificatesByCA where
        toQuery ListCertificatesByCA'{..}
          = mconcat
              ["marker" =: _lcbcaMarker,
               "isAscendingOrder" =: _lcbcaAscendingOrder,
               "pageSize" =: _lcbcaPageSize]

-- | The output of the ListCertificatesByCA operation.
--
--
--
-- /See:/ 'listCertificatesByCAResponse' smart constructor.
data ListCertificatesByCAResponse = ListCertificatesByCAResponse'
  { _lcbcarsCertificates   :: !(Maybe [Certificate])
  , _lcbcarsNextMarker     :: !(Maybe Text)
  , _lcbcarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListCertificatesByCAResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcbcarsCertificates' - The device certificates signed by the specified CA certificate.
--
-- * 'lcbcarsNextMarker' - The marker for the next set of results, or null if there are no additional results.
--
-- * 'lcbcarsResponseStatus' - -- | The response status code.
listCertificatesByCAResponse
    :: Int -- ^ 'lcbcarsResponseStatus'
    -> ListCertificatesByCAResponse
listCertificatesByCAResponse pResponseStatus_ =
  ListCertificatesByCAResponse'
    { _lcbcarsCertificates = Nothing
    , _lcbcarsNextMarker = Nothing
    , _lcbcarsResponseStatus = pResponseStatus_
    }


-- | The device certificates signed by the specified CA certificate.
lcbcarsCertificates :: Lens' ListCertificatesByCAResponse [Certificate]
lcbcarsCertificates = lens _lcbcarsCertificates (\ s a -> s{_lcbcarsCertificates = a}) . _Default . _Coerce

-- | The marker for the next set of results, or null if there are no additional results.
lcbcarsNextMarker :: Lens' ListCertificatesByCAResponse (Maybe Text)
lcbcarsNextMarker = lens _lcbcarsNextMarker (\ s a -> s{_lcbcarsNextMarker = a})

-- | -- | The response status code.
lcbcarsResponseStatus :: Lens' ListCertificatesByCAResponse Int
lcbcarsResponseStatus = lens _lcbcarsResponseStatus (\ s a -> s{_lcbcarsResponseStatus = a})

instance NFData ListCertificatesByCAResponse where
