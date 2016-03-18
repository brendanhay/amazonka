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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of the ACM Certificate ARNs, and the domain name for
-- each ARN, owned by the calling account. You can filter the list based on
-- the 'CertificateStatuses' parameter, and you can display up to
-- 'MaxItems' certificates at one time. If you have more than 'MaxItems'
-- certificates, use the 'NextToken' marker from the response object in
-- your next call to the 'ListCertificates' action to retrieve the next set
-- of certificate ARNs.
module Network.AWS.CertificateManager.ListCertificates
    (
    -- * Creating a Request
      listCertificates
    , ListCertificates
    -- * Request Lenses
    , lcCertificateStatuses
    , lcNextToken
    , lcMaxItems

    -- * Destructuring the Response
    , listCertificatesResponse
    , ListCertificatesResponse
    -- * Response Lenses
    , lcrsCertificateSummaryList
    , lcrsNextToken
    , lcrsResponseStatus
    ) where

import           Network.AWS.CertificateManager.Types
import           Network.AWS.CertificateManager.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listCertificates' smart constructor.
data ListCertificates = ListCertificates'
    { _lcCertificateStatuses :: !(Maybe [CertificateStatus])
    , _lcNextToken           :: !(Maybe Text)
    , _lcMaxItems            :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListCertificates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcCertificateStatuses'
--
-- * 'lcNextToken'
--
-- * 'lcMaxItems'
listCertificates
    :: ListCertificates
listCertificates =
    ListCertificates'
    { _lcCertificateStatuses = Nothing
    , _lcNextToken = Nothing
    , _lcMaxItems = Nothing
    }

-- | Identifies the statuses of the ACM Certificates for which you want to
-- retrieve the ARNs. This can be one or more of the following values:
--
-- -   'PENDING_VALIDATION'
-- -   'ISSUED'
-- -   'INACTIVE'
-- -   'EXPIRED'
-- -   'VALIDATION_TIMED_OUT'
-- -   'REVOKED'
-- -   'FAILED'
lcCertificateStatuses :: Lens' ListCertificates [CertificateStatus]
lcCertificateStatuses = lens _lcCertificateStatuses (\ s a -> s{_lcCertificateStatuses = a}) . _Default . _Coerce;

-- | String that contains an opaque marker of the next ACM Certificate ARN to
-- be displayed. Use this parameter when paginating results, and only in a
-- subsequent request after you\'ve received a response where the results
-- have been truncated. Set it to an empty string the first time you call
-- this action, and set it to the value of the 'NextToken' element you
-- receive in the response object for subsequent calls.
lcNextToken :: Lens' ListCertificates (Maybe Text)
lcNextToken = lens _lcNextToken (\ s a -> s{_lcNextToken = a});

-- | Specify this parameter when paginating results to indicate the maximum
-- number of ACM Certificates that you want to display for each response.
-- If there are additional certificates beyond the maximum you specify, use
-- the 'NextToken' value in your next call to the 'ListCertificates'
-- action.
lcMaxItems :: Lens' ListCertificates (Maybe Natural)
lcMaxItems = lens _lcMaxItems (\ s a -> s{_lcMaxItems = a}) . mapping _Nat;

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListCertificatesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcrsCertificateSummaryList'
--
-- * 'lcrsNextToken'
--
-- * 'lcrsResponseStatus'
listCertificatesResponse
    :: Int -- ^ 'lcrsResponseStatus'
    -> ListCertificatesResponse
listCertificatesResponse pResponseStatus_ =
    ListCertificatesResponse'
    { _lcrsCertificateSummaryList = Nothing
    , _lcrsNextToken = Nothing
    , _lcrsResponseStatus = pResponseStatus_
    }

-- | A list of the certificate ARNs.
lcrsCertificateSummaryList :: Lens' ListCertificatesResponse [CertificateSummary]
lcrsCertificateSummaryList = lens _lcrsCertificateSummaryList (\ s a -> s{_lcrsCertificateSummaryList = a}) . _Default . _Coerce;

-- | If the list has been truncated, this value is present and should be used
-- for the 'NextToken' input parameter on your next call to
-- 'ListCertificates'.
lcrsNextToken :: Lens' ListCertificatesResponse (Maybe Text)
lcrsNextToken = lens _lcrsNextToken (\ s a -> s{_lcrsNextToken = a});

-- | The response status code.
lcrsResponseStatus :: Lens' ListCertificatesResponse Int
lcrsResponseStatus = lens _lcrsResponseStatus (\ s a -> s{_lcrsResponseStatus = a});
