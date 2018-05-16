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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests an ACM certificate for use with other AWS services. To request an ACM certificate, you must specify a fully qualified domain name (FQDN) in the @DomainName@ parameter. You can also specify additional FQDNs in the @SubjectAlternativeNames@ parameter.
--
--
-- If you are requesting a private certificate, domain validation is not required. If you are requesting a public certificate, each domain name that you specify must be validated to verify that you own or control the domain. You can use <http://docs.aws.amazon.com/acm/latest/userguide/gs-acm-validate-dns.html DNS validation> or <http://docs.aws.amazon.com/acm/latest/userguide/gs-acm-validate-email.html email validation> . We recommend that you use DNS validation. ACM issues public certificates after receiving approval from the domain owner.
--
module Network.AWS.CertificateManager.RequestCertificate
    (
    -- * Creating a Request
      requestCertificate
    , RequestCertificate
    -- * Request Lenses
    , rcIdempotencyToken
    , rcValidationMethod
    , rcSubjectAlternativeNames
    , rcOptions
    , rcDomainValidationOptions
    , rcCertificateAuthorityARN
    , rcDomainName

    -- * Destructuring the Response
    , requestCertificateResponse
    , RequestCertificateResponse
    -- * Response Lenses
    , rcrsCertificateARN
    , rcrsResponseStatus
    ) where

import Network.AWS.CertificateManager.Types
import Network.AWS.CertificateManager.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'requestCertificate' smart constructor.
data RequestCertificate = RequestCertificate'
  { _rcIdempotencyToken        :: !(Maybe Text)
  , _rcValidationMethod        :: !(Maybe ValidationMethod)
  , _rcSubjectAlternativeNames :: !(Maybe (List1 Text))
  , _rcOptions                 :: !(Maybe CertificateOptions)
  , _rcDomainValidationOptions :: !(Maybe (List1 DomainValidationOption))
  , _rcCertificateAuthorityARN :: !(Maybe Text)
  , _rcDomainName              :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RequestCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcIdempotencyToken' - Customer chosen string that can be used to distinguish between calls to @RequestCertificate@ . Idempotency tokens time out after one hour. Therefore, if you call @RequestCertificate@ multiple times with the same idempotency token within one hour, ACM recognizes that you are requesting only one certificate and will issue only one. If you change the idempotency token for each call, ACM recognizes that you are requesting multiple certificates.
--
-- * 'rcValidationMethod' - The method you want to use if you are requesting a public certificate to validate that you own or control domain. You can <http://docs.aws.amazon.com/acm/latest/userguide/gs-acm-validate-dns.html validate with DNS> or <http://docs.aws.amazon.com/acm/latest/userguide/gs-acm-validate-email.html validate with email> . We recommend that you use DNS validation.
--
-- * 'rcSubjectAlternativeNames' - Additional FQDNs to be included in the Subject Alternative Name extension of the ACM certificate. For example, add the name www.example.net to a certificate for which the @DomainName@ field is www.example.com if users can reach your site by using either name. The maximum number of domain names that you can add to an ACM certificate is 100. However, the initial limit is 10 domain names. If you need more than 10 names, you must request a limit increase. For more information, see <http://docs.aws.amazon.com/acm/latest/userguide/acm-limits.html Limits> . The maximum length of a SAN DNS name is 253 octets. The name is made up of multiple labels separated by periods. No label can be longer than 63 octets. Consider the following examples:      * @(63 octets).(63 octets).(63 octets).(61 octets)@ is legal because the total length is 253 octets (63+1+63+1+63+1+61) and no label exceeds 63 octets.     * @(64 octets).(63 octets).(63 octets).(61 octets)@ is not legal because the total length exceeds 253 octets (64+1+63+1+63+1+61) and the first label exceeds 63 octets.     * @(63 octets).(63 octets).(63 octets).(62 octets)@ is not legal because the total length of the DNS name (63+1+63+1+63+1+62) exceeds 253 octets.
--
-- * 'rcOptions' - Currently, you can use this parameter to specify whether to add the certificate to a certificate transparency log. Certificate transparency makes it possible to detect SSL/TLS certificates that have been mistakenly or maliciously issued. Certificates that have not been logged typically produce an error message in a browser. For more information, see <http://docs.aws.amazon.com/acm/latest/userguide/acm-bestpractices.html#best-practices-transparency Opting Out of Certificate Transparency Logging> .
--
-- * 'rcDomainValidationOptions' - The domain name that you want ACM to use to send you emails so that you can validate domain ownership.
--
-- * 'rcCertificateAuthorityARN' - The Amazon Resource Name (ARN) of the private certificate authority (CA) that will be used to issue the certificate. If you do not provide an ARN and you are trying to request a private certificate, ACM will attempt to issue a public certificate. For more information about private CAs, see the <http://docs.aws.amazon.com/acm-pca/latest/userguide/PcaWelcome.html AWS Certificate Manager Private Certificate Authority (PCA)> user guide. The ARN must have the following form:  @arn:aws:acm-pca:region:account:certificate-authority/12345678-1234-1234-1234-123456789012@
--
-- * 'rcDomainName' - Fully qualified domain name (FQDN), such as www.example.com, that you want to secure with an ACM certificate. Use an asterisk (*) to create a wildcard certificate that protects several sites in the same domain. For example, *.example.com protects www.example.com, site.example.com, and images.example.com.  The first domain name you enter cannot exceed 63 octets, including periods. Each subsequent Subject Alternative Name (SAN), however, can be up to 253 octets in length.
requestCertificate
    :: Text -- ^ 'rcDomainName'
    -> RequestCertificate
requestCertificate pDomainName_ =
  RequestCertificate'
    { _rcIdempotencyToken = Nothing
    , _rcValidationMethod = Nothing
    , _rcSubjectAlternativeNames = Nothing
    , _rcOptions = Nothing
    , _rcDomainValidationOptions = Nothing
    , _rcCertificateAuthorityARN = Nothing
    , _rcDomainName = pDomainName_
    }


-- | Customer chosen string that can be used to distinguish between calls to @RequestCertificate@ . Idempotency tokens time out after one hour. Therefore, if you call @RequestCertificate@ multiple times with the same idempotency token within one hour, ACM recognizes that you are requesting only one certificate and will issue only one. If you change the idempotency token for each call, ACM recognizes that you are requesting multiple certificates.
rcIdempotencyToken :: Lens' RequestCertificate (Maybe Text)
rcIdempotencyToken = lens _rcIdempotencyToken (\ s a -> s{_rcIdempotencyToken = a})

-- | The method you want to use if you are requesting a public certificate to validate that you own or control domain. You can <http://docs.aws.amazon.com/acm/latest/userguide/gs-acm-validate-dns.html validate with DNS> or <http://docs.aws.amazon.com/acm/latest/userguide/gs-acm-validate-email.html validate with email> . We recommend that you use DNS validation.
rcValidationMethod :: Lens' RequestCertificate (Maybe ValidationMethod)
rcValidationMethod = lens _rcValidationMethod (\ s a -> s{_rcValidationMethod = a})

-- | Additional FQDNs to be included in the Subject Alternative Name extension of the ACM certificate. For example, add the name www.example.net to a certificate for which the @DomainName@ field is www.example.com if users can reach your site by using either name. The maximum number of domain names that you can add to an ACM certificate is 100. However, the initial limit is 10 domain names. If you need more than 10 names, you must request a limit increase. For more information, see <http://docs.aws.amazon.com/acm/latest/userguide/acm-limits.html Limits> . The maximum length of a SAN DNS name is 253 octets. The name is made up of multiple labels separated by periods. No label can be longer than 63 octets. Consider the following examples:      * @(63 octets).(63 octets).(63 octets).(61 octets)@ is legal because the total length is 253 octets (63+1+63+1+63+1+61) and no label exceeds 63 octets.     * @(64 octets).(63 octets).(63 octets).(61 octets)@ is not legal because the total length exceeds 253 octets (64+1+63+1+63+1+61) and the first label exceeds 63 octets.     * @(63 octets).(63 octets).(63 octets).(62 octets)@ is not legal because the total length of the DNS name (63+1+63+1+63+1+62) exceeds 253 octets.
rcSubjectAlternativeNames :: Lens' RequestCertificate (Maybe (NonEmpty Text))
rcSubjectAlternativeNames = lens _rcSubjectAlternativeNames (\ s a -> s{_rcSubjectAlternativeNames = a}) . mapping _List1

-- | Currently, you can use this parameter to specify whether to add the certificate to a certificate transparency log. Certificate transparency makes it possible to detect SSL/TLS certificates that have been mistakenly or maliciously issued. Certificates that have not been logged typically produce an error message in a browser. For more information, see <http://docs.aws.amazon.com/acm/latest/userguide/acm-bestpractices.html#best-practices-transparency Opting Out of Certificate Transparency Logging> .
rcOptions :: Lens' RequestCertificate (Maybe CertificateOptions)
rcOptions = lens _rcOptions (\ s a -> s{_rcOptions = a})

-- | The domain name that you want ACM to use to send you emails so that you can validate domain ownership.
rcDomainValidationOptions :: Lens' RequestCertificate (Maybe (NonEmpty DomainValidationOption))
rcDomainValidationOptions = lens _rcDomainValidationOptions (\ s a -> s{_rcDomainValidationOptions = a}) . mapping _List1

-- | The Amazon Resource Name (ARN) of the private certificate authority (CA) that will be used to issue the certificate. If you do not provide an ARN and you are trying to request a private certificate, ACM will attempt to issue a public certificate. For more information about private CAs, see the <http://docs.aws.amazon.com/acm-pca/latest/userguide/PcaWelcome.html AWS Certificate Manager Private Certificate Authority (PCA)> user guide. The ARN must have the following form:  @arn:aws:acm-pca:region:account:certificate-authority/12345678-1234-1234-1234-123456789012@
rcCertificateAuthorityARN :: Lens' RequestCertificate (Maybe Text)
rcCertificateAuthorityARN = lens _rcCertificateAuthorityARN (\ s a -> s{_rcCertificateAuthorityARN = a})

-- | Fully qualified domain name (FQDN), such as www.example.com, that you want to secure with an ACM certificate. Use an asterisk (*) to create a wildcard certificate that protects several sites in the same domain. For example, *.example.com protects www.example.com, site.example.com, and images.example.com.  The first domain name you enter cannot exceed 63 octets, including periods. Each subsequent Subject Alternative Name (SAN), however, can be up to 253 octets in length.
rcDomainName :: Lens' RequestCertificate Text
rcDomainName = lens _rcDomainName (\ s a -> s{_rcDomainName = a})

instance AWSRequest RequestCertificate where
        type Rs RequestCertificate =
             RequestCertificateResponse
        request = postJSON certificateManager
        response
          = receiveJSON
              (\ s h x ->
                 RequestCertificateResponse' <$>
                   (x .?> "CertificateArn") <*> (pure (fromEnum s)))

instance Hashable RequestCertificate where

instance NFData RequestCertificate where

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
                  ("ValidationMethod" .=) <$> _rcValidationMethod,
                  ("SubjectAlternativeNames" .=) <$>
                    _rcSubjectAlternativeNames,
                  ("Options" .=) <$> _rcOptions,
                  ("DomainValidationOptions" .=) <$>
                    _rcDomainValidationOptions,
                  ("CertificateAuthorityArn" .=) <$>
                    _rcCertificateAuthorityARN,
                  Just ("DomainName" .= _rcDomainName)])

instance ToPath RequestCertificate where
        toPath = const "/"

instance ToQuery RequestCertificate where
        toQuery = const mempty

-- | /See:/ 'requestCertificateResponse' smart constructor.
data RequestCertificateResponse = RequestCertificateResponse'
  { _rcrsCertificateARN :: !(Maybe Text)
  , _rcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
    {_rcrsCertificateARN = Nothing, _rcrsResponseStatus = pResponseStatus_}


-- | String that contains the ARN of the issued certificate. This must be of the form: @arn:aws:acm:us-east-1:123456789012:certificate/12345678-1234-1234-1234-123456789012@
rcrsCertificateARN :: Lens' RequestCertificateResponse (Maybe Text)
rcrsCertificateARN = lens _rcrsCertificateARN (\ s a -> s{_rcrsCertificateARN = a})

-- | -- | The response status code.
rcrsResponseStatus :: Lens' RequestCertificateResponse Int
rcrsResponseStatus = lens _rcrsResponseStatus (\ s a -> s{_rcrsResponseStatus = a})

instance NFData RequestCertificateResponse where
