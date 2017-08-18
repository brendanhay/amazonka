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
-- Module      : Network.AWS.CertificateManager.RequestCertificate
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests an ACM Certificate for use with other AWS services. To request an ACM Certificate, you must specify the fully qualified domain name (FQDN) for your site. You can also specify additional FQDNs if users can reach your site by using other names. For each domain name you specify, email is sent to the domain owner to request approval to issue the certificate. After receiving approval from the domain owner, the ACM Certificate is issued. For more information, see the <http://docs.aws.amazon.com/acm/latest/userguide/ AWS Certificate Manager User Guide> .
--
--
module Network.AWS.CertificateManager.RequestCertificate
    (
    -- * Creating a Request
      requestCertificate
    , RequestCertificate
    -- * Request Lenses
    , rcIdempotencyToken
    , rcSubjectAlternativeNames
    , rcDomainValidationOptions
    , rcDomainName

    -- * Destructuring the Response
    , requestCertificateResponse
    , RequestCertificateResponse
    -- * Response Lenses
    , rcrsCertificateARN
    , rcrsResponseStatus
    ) where

import           Network.AWS.CertificateManager.Types
import           Network.AWS.CertificateManager.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'requestCertificate' smart constructor.
data RequestCertificate = RequestCertificate'
    { _rcIdempotencyToken        :: !(Maybe Text)
    , _rcSubjectAlternativeNames :: !(Maybe (List1 Text))
    , _rcDomainValidationOptions :: !(Maybe (List1 DomainValidationOption))
    , _rcDomainName              :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RequestCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcIdempotencyToken' - Customer chosen string that can be used to distinguish between calls to @RequestCertificate@ . Idempotency tokens time out after one hour. Therefore, if you call @RequestCertificate@ multiple times with the same idempotency token within one hour, ACM recognizes that you are requesting only one certificate and will issue only one. If you change the idempotency token for each call, ACM recognizes that you are requesting multiple certificates.
--
-- * 'rcSubjectAlternativeNames' - Additional FQDNs to be included in the Subject Alternative Name extension of the ACM Certificate. For example, add the name www.example.net to a certificate for which the @DomainName@ field is www.example.com if users can reach your site by using either name. The maximum number of domain names that you can add to an ACM Certificate is 100. However, the initial limit is 10 domain names. If you need more than 10 names, you must request a limit increase. For more information, see <http://docs.aws.amazon.com/acm/latest/userguide/acm-limits.html Limits> .
--
-- * 'rcDomainValidationOptions' - The domain name that you want ACM to use to send you emails to validate your ownership of the domain.
--
-- * 'rcDomainName' - Fully qualified domain name (FQDN), such as www.example.com, of the site that you want to secure with an ACM Certificate. Use an asterisk (*) to create a wildcard certificate that protects several sites in the same domain. For example, *.example.com protects www.example.com, site.example.com, and images.example.com.  The maximum length of a DNS name is 253 octets. The name is made up of multiple labels separated by periods. No label can be longer than 63 octets. Consider the following examples:  @(63 octets).(63 octets).(63 octets).(61 octets)@ is legal because the total length is 253 octets (63+1+63+1+63+1+61) and no label exceeds 63 octets.  @(64 octets).(63 octets).(63 octets).(61 octets)@ is not legal because the total length exceeds 253 octets (64+1+63+1+63+1+61) and the first label exceeds 63 octets.  @(63 octets).(63 octets).(63 octets).(62 octets)@ is not legal because the total length of the DNS name (63+1+63+1+63+1+62) exceeds 253 octets.
requestCertificate
    :: Text -- ^ 'rcDomainName'
    -> RequestCertificate
requestCertificate pDomainName_ =
    RequestCertificate'
    { _rcIdempotencyToken = Nothing
    , _rcSubjectAlternativeNames = Nothing
    , _rcDomainValidationOptions = Nothing
    , _rcDomainName = pDomainName_
    }

-- | Customer chosen string that can be used to distinguish between calls to @RequestCertificate@ . Idempotency tokens time out after one hour. Therefore, if you call @RequestCertificate@ multiple times with the same idempotency token within one hour, ACM recognizes that you are requesting only one certificate and will issue only one. If you change the idempotency token for each call, ACM recognizes that you are requesting multiple certificates.
rcIdempotencyToken :: Lens' RequestCertificate (Maybe Text)
rcIdempotencyToken = lens _rcIdempotencyToken (\ s a -> s{_rcIdempotencyToken = a});

-- | Additional FQDNs to be included in the Subject Alternative Name extension of the ACM Certificate. For example, add the name www.example.net to a certificate for which the @DomainName@ field is www.example.com if users can reach your site by using either name. The maximum number of domain names that you can add to an ACM Certificate is 100. However, the initial limit is 10 domain names. If you need more than 10 names, you must request a limit increase. For more information, see <http://docs.aws.amazon.com/acm/latest/userguide/acm-limits.html Limits> .
rcSubjectAlternativeNames :: Lens' RequestCertificate (Maybe (NonEmpty Text))
rcSubjectAlternativeNames = lens _rcSubjectAlternativeNames (\ s a -> s{_rcSubjectAlternativeNames = a}) . mapping _List1;

-- | The domain name that you want ACM to use to send you emails to validate your ownership of the domain.
rcDomainValidationOptions :: Lens' RequestCertificate (Maybe (NonEmpty DomainValidationOption))
rcDomainValidationOptions = lens _rcDomainValidationOptions (\ s a -> s{_rcDomainValidationOptions = a}) . mapping _List1;

-- | Fully qualified domain name (FQDN), such as www.example.com, of the site that you want to secure with an ACM Certificate. Use an asterisk (*) to create a wildcard certificate that protects several sites in the same domain. For example, *.example.com protects www.example.com, site.example.com, and images.example.com.  The maximum length of a DNS name is 253 octets. The name is made up of multiple labels separated by periods. No label can be longer than 63 octets. Consider the following examples:  @(63 octets).(63 octets).(63 octets).(61 octets)@ is legal because the total length is 253 octets (63+1+63+1+63+1+61) and no label exceeds 63 octets.  @(64 octets).(63 octets).(63 octets).(61 octets)@ is not legal because the total length exceeds 253 octets (64+1+63+1+63+1+61) and the first label exceeds 63 octets.  @(63 octets).(63 octets).(63 octets).(62 octets)@ is not legal because the total length of the DNS name (63+1+63+1+63+1+62) exceeds 253 octets.
rcDomainName :: Lens' RequestCertificate Text
rcDomainName = lens _rcDomainName (\ s a -> s{_rcDomainName = a});

instance AWSRequest RequestCertificate where
        type Rs RequestCertificate =
             RequestCertificateResponse
        request = postJSON certificateManager
        response
          = receiveJSON
              (\ s h x ->
                 RequestCertificateResponse' <$>
                   (x .?> "CertificateArn") <*> (pure (fromEnum s)))

instance Hashable RequestCertificate

instance NFData RequestCertificate

instance ToHeaders RequestCertificate where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CertificateManager.RequestCertificate" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RequestCertificate where
        toJSON RequestCertificate'{..}
          = object
              (catMaybes
                 [("IdempotencyToken" .=) <$> _rcIdempotencyToken,
                  ("SubjectAlternativeNames" .=) <$>
                    _rcSubjectAlternativeNames,
                  ("DomainValidationOptions" .=) <$>
                    _rcDomainValidationOptions,
                  Just ("DomainName" .= _rcDomainName)])

instance ToPath RequestCertificate where
        toPath = const "/"

instance ToQuery RequestCertificate where
        toQuery = const mempty

-- | /See:/ 'requestCertificateResponse' smart constructor.
data RequestCertificateResponse = RequestCertificateResponse'
    { _rcrsCertificateARN :: !(Maybe Text)
    , _rcrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RequestCertificateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcrsCertificateARN' - String that contains the ARN of the issued certificate. This must be of the form: @arn:aws:acm:us-east-1:123456789012:certificate/12345678-1234-1234-1234-123456789012@
--
-- * 'rcrsResponseStatus' - -- | The response status code.
requestCertificateResponse
    :: Int -- ^ 'rcrsResponseStatus'
    -> RequestCertificateResponse
requestCertificateResponse pResponseStatus_ =
    RequestCertificateResponse'
    { _rcrsCertificateARN = Nothing
    , _rcrsResponseStatus = pResponseStatus_
    }

-- | String that contains the ARN of the issued certificate. This must be of the form: @arn:aws:acm:us-east-1:123456789012:certificate/12345678-1234-1234-1234-123456789012@
rcrsCertificateARN :: Lens' RequestCertificateResponse (Maybe Text)
rcrsCertificateARN = lens _rcrsCertificateARN (\ s a -> s{_rcrsCertificateARN = a});

-- | -- | The response status code.
rcrsResponseStatus :: Lens' RequestCertificateResponse Int
rcrsResponseStatus = lens _rcrsResponseStatus (\ s a -> s{_rcrsResponseStatus = a});

instance NFData RequestCertificateResponse
