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
-- Module      : Network.AWS.CertificateManager.ListCertificates
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of certificate ARNs and domain names. You can request that only certificates that match a specific status be listed. You can also filter by specific attributes of the certificate.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CertificateManager.ListCertificates
    (
    -- * Creating a Request
      listCertificates
    , ListCertificates
    -- * Request Lenses
    , lcCertificateStatuses
    , lcNextToken
    , lcIncludes
    , lcMaxItems

    -- * Destructuring the Response
    , listCertificatesResponse
    , ListCertificatesResponse
    -- * Response Lenses
    , lcrsCertificateSummaryList
    , lcrsNextToken
    , lcrsResponseStatus
    ) where

import Network.AWS.CertificateManager.Types
import Network.AWS.CertificateManager.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listCertificates' smart constructor.
data ListCertificates = ListCertificates'
  { _lcCertificateStatuses :: !(Maybe [CertificateStatus])
  , _lcNextToken           :: !(Maybe Text)
  , _lcIncludes            :: !(Maybe Filters)
  , _lcMaxItems            :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListCertificates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcCertificateStatuses' - Filter the certificate list by status value.
--
-- * 'lcNextToken' - Use this parameter only when paginating results and only in a subsequent request after you receive a response with truncated results. Set it to the value of @NextToken@ from the response you just received.
--
-- * 'lcIncludes' - Filter the certificate list. For more information, see the 'Filters' structure.
--
-- * 'lcMaxItems' - Use this parameter when paginating results to specify the maximum number of items to return in the response. If additional items exist beyond the number you specify, the @NextToken@ element is sent in the response. Use this @NextToken@ value in a subsequent request to retrieve additional items.
listCertificates
    :: ListCertificates
listCertificates =
  ListCertificates'
    { _lcCertificateStatuses = Nothing
    , _lcNextToken = Nothing
    , _lcIncludes = Nothing
    , _lcMaxItems = Nothing
    }


-- | Filter the certificate list by status value.
lcCertificateStatuses :: Lens' ListCertificates [CertificateStatus]
lcCertificateStatuses = lens _lcCertificateStatuses (\ s a -> s{_lcCertificateStatuses = a}) . _Default . _Coerce

-- | Use this parameter only when paginating results and only in a subsequent request after you receive a response with truncated results. Set it to the value of @NextToken@ from the response you just received.
lcNextToken :: Lens' ListCertificates (Maybe Text)
lcNextToken = lens _lcNextToken (\ s a -> s{_lcNextToken = a})

-- | Filter the certificate list. For more information, see the 'Filters' structure.
lcIncludes :: Lens' ListCertificates (Maybe Filters)
lcIncludes = lens _lcIncludes (\ s a -> s{_lcIncludes = a})

-- | Use this parameter when paginating results to specify the maximum number of items to return in the response. If additional items exist beyond the number you specify, the @NextToken@ element is sent in the response. Use this @NextToken@ value in a subsequent request to retrieve additional items.
lcMaxItems :: Lens' ListCertificates (Maybe Natural)
lcMaxItems = lens _lcMaxItems (\ s a -> s{_lcMaxItems = a}) . mapping _Nat

instance AWSPager ListCertificates where
        page rq rs
          | stop (rs ^. lcrsNextToken) = Nothing
          | stop (rs ^. lcrsCertificateSummaryList) = Nothing
          | otherwise =
            Just $ rq & lcNextToken .~ rs ^. lcrsNextToken

instance AWSRequest ListCertificates where
        type Rs ListCertificates = ListCertificatesResponse
        request = postJSON certificateManager
        response
          = receiveJSON
              (\ s h x ->
                 ListCertificatesResponse' <$>
                   (x .?> "CertificateSummaryList" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListCertificates where

instance NFData ListCertificates where

instance ToHeaders ListCertificates where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CertificateManager.ListCertificates" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListCertificates where
        toJSON ListCertificates'{..}
          = object
              (catMaybes
                 [("CertificateStatuses" .=) <$>
                    _lcCertificateStatuses,
                  ("NextToken" .=) <$> _lcNextToken,
                  ("Includes" .=) <$> _lcIncludes,
                  ("MaxItems" .=) <$> _lcMaxItems])

instance ToPath ListCertificates where
        toPath = const "/"

instance ToQuery ListCertificates where
        toQuery = const mempty

-- | /See:/ 'listCertificatesResponse' smart constructor.
data ListCertificatesResponse = ListCertificatesResponse'
  { _lcrsCertificateSummaryList :: !(Maybe [CertificateSummary])
  , _lcrsNextToken              :: !(Maybe Text)
  , _lcrsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListCertificatesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcrsCertificateSummaryList' - A list of ACM certificates.
--
-- * 'lcrsNextToken' - When the list is truncated, this value is present and contains the value to use for the @NextToken@ parameter in a subsequent pagination request.
--
-- * 'lcrsResponseStatus' - -- | The response status code.
listCertificatesResponse
    :: Int -- ^ 'lcrsResponseStatus'
    -> ListCertificatesResponse
listCertificatesResponse pResponseStatus_ =
  ListCertificatesResponse'
    { _lcrsCertificateSummaryList = Nothing
    , _lcrsNextToken = Nothing
    , _lcrsResponseStatus = pResponseStatus_
    }


-- | A list of ACM certificates.
lcrsCertificateSummaryList :: Lens' ListCertificatesResponse [CertificateSummary]
lcrsCertificateSummaryList = lens _lcrsCertificateSummaryList (\ s a -> s{_lcrsCertificateSummaryList = a}) . _Default . _Coerce

-- | When the list is truncated, this value is present and contains the value to use for the @NextToken@ parameter in a subsequent pagination request.
lcrsNextToken :: Lens' ListCertificatesResponse (Maybe Text)
lcrsNextToken = lens _lcrsNextToken (\ s a -> s{_lcrsNextToken = a})

-- | -- | The response status code.
lcrsResponseStatus :: Lens' ListCertificatesResponse Int
lcrsResponseStatus = lens _lcrsResponseStatus (\ s a -> s{_lcrsResponseStatus = a})

instance NFData ListCertificatesResponse where
