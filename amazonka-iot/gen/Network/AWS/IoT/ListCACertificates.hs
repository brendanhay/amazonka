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
-- Module      : Network.AWS.IoT.ListCACertificates
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the CA certificates registered for your AWS account.
--
--
-- The results are paginated with a default page size of 25. You can use the returned marker to retrieve additional results.
--
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListCACertificates
    (
    -- * Creating a Request
      listCACertificates
    , ListCACertificates
    -- * Request Lenses
    , lcacMarker
    , lcacAscendingOrder
    , lcacPageSize

    -- * Destructuring the Response
    , listCACertificatesResponse
    , ListCACertificatesResponse
    -- * Response Lenses
    , lcacrsCertificates
    , lcacrsNextMarker
    , lcacrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Input for the ListCACertificates operation.
--
--
--
-- /See:/ 'listCACertificates' smart constructor.
data ListCACertificates = ListCACertificates'
  { _lcacMarker         :: !(Maybe Text)
  , _lcacAscendingOrder :: !(Maybe Bool)
  , _lcacPageSize       :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListCACertificates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcacMarker' - The marker for the next set of results.
--
-- * 'lcacAscendingOrder' - Determines the order of the results.
--
-- * 'lcacPageSize' - The result page size.
listCACertificates
    :: ListCACertificates
listCACertificates =
  ListCACertificates'
    { _lcacMarker = Nothing
    , _lcacAscendingOrder = Nothing
    , _lcacPageSize = Nothing
    }


-- | The marker for the next set of results.
lcacMarker :: Lens' ListCACertificates (Maybe Text)
lcacMarker = lens _lcacMarker (\ s a -> s{_lcacMarker = a})

-- | Determines the order of the results.
lcacAscendingOrder :: Lens' ListCACertificates (Maybe Bool)
lcacAscendingOrder = lens _lcacAscendingOrder (\ s a -> s{_lcacAscendingOrder = a})

-- | The result page size.
lcacPageSize :: Lens' ListCACertificates (Maybe Natural)
lcacPageSize = lens _lcacPageSize (\ s a -> s{_lcacPageSize = a}) . mapping _Nat

instance AWSPager ListCACertificates where
        page rq rs
          | stop (rs ^. lcacrsNextMarker) = Nothing
          | stop (rs ^. lcacrsCertificates) = Nothing
          | otherwise =
            Just $ rq & lcacMarker .~ rs ^. lcacrsNextMarker

instance AWSRequest ListCACertificates where
        type Rs ListCACertificates =
             ListCACertificatesResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 ListCACertificatesResponse' <$>
                   (x .?> "certificates" .!@ mempty) <*>
                     (x .?> "nextMarker")
                     <*> (pure (fromEnum s)))

instance Hashable ListCACertificates where

instance NFData ListCACertificates where

instance ToHeaders ListCACertificates where
        toHeaders = const mempty

instance ToPath ListCACertificates where
        toPath = const "/cacertificates"

instance ToQuery ListCACertificates where
        toQuery ListCACertificates'{..}
          = mconcat
              ["marker" =: _lcacMarker,
               "isAscendingOrder" =: _lcacAscendingOrder,
               "pageSize" =: _lcacPageSize]

-- | The output from the ListCACertificates operation.
--
--
--
-- /See:/ 'listCACertificatesResponse' smart constructor.
data ListCACertificatesResponse = ListCACertificatesResponse'
  { _lcacrsCertificates   :: !(Maybe [CACertificate])
  , _lcacrsNextMarker     :: !(Maybe Text)
  , _lcacrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListCACertificatesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcacrsCertificates' - The CA certificates registered in your AWS account.
--
-- * 'lcacrsNextMarker' - The current position within the list of CA certificates.
--
-- * 'lcacrsResponseStatus' - -- | The response status code.
listCACertificatesResponse
    :: Int -- ^ 'lcacrsResponseStatus'
    -> ListCACertificatesResponse
listCACertificatesResponse pResponseStatus_ =
  ListCACertificatesResponse'
    { _lcacrsCertificates = Nothing
    , _lcacrsNextMarker = Nothing
    , _lcacrsResponseStatus = pResponseStatus_
    }


-- | The CA certificates registered in your AWS account.
lcacrsCertificates :: Lens' ListCACertificatesResponse [CACertificate]
lcacrsCertificates = lens _lcacrsCertificates (\ s a -> s{_lcacrsCertificates = a}) . _Default . _Coerce

-- | The current position within the list of CA certificates.
lcacrsNextMarker :: Lens' ListCACertificatesResponse (Maybe Text)
lcacrsNextMarker = lens _lcacrsNextMarker (\ s a -> s{_lcacrsNextMarker = a})

-- | -- | The response status code.
lcacrsResponseStatus :: Lens' ListCACertificatesResponse Int
lcacrsResponseStatus = lens _lcacrsResponseStatus (\ s a -> s{_lcacrsResponseStatus = a})

instance NFData ListCACertificatesResponse where
