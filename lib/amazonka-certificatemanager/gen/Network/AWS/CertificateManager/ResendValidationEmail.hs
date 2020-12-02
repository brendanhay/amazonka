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
-- Module      : Network.AWS.CertificateManager.ResendValidationEmail
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resends the email that requests domain ownership validation. The domain owner or an authorized representative must approve the ACM certificate before it can be issued. The certificate can be approved by clicking a link in the mail to navigate to the Amazon certificate approval website and then clicking __I Approve__ . However, the validation email can be blocked by spam filters. Therefore, if you do not receive the original mail, you can request that the mail be resent within 72 hours of requesting the ACM certificate. If more than 72 hours have elapsed since your original request or since your last attempt to resend validation mail, you must request a new certificate. For more information about setting up your contact email addresses, see <http://docs.aws.amazon.com/acm/latest/userguide/setup-email.html Configure Email for your Domain> .
--
--
module Network.AWS.CertificateManager.ResendValidationEmail
    (
    -- * Creating a Request
      resendValidationEmail
    , ResendValidationEmail
    -- * Request Lenses
    , rveCertificateARN
    , rveDomain
    , rveValidationDomain

    -- * Destructuring the Response
    , resendValidationEmailResponse
    , ResendValidationEmailResponse
    ) where

import Network.AWS.CertificateManager.Types
import Network.AWS.CertificateManager.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'resendValidationEmail' smart constructor.
data ResendValidationEmail = ResendValidationEmail'
  { _rveCertificateARN   :: !Text
  , _rveDomain           :: !Text
  , _rveValidationDomain :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResendValidationEmail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rveCertificateARN' - String that contains the ARN of the requested certificate. The certificate ARN is generated and returned by the 'RequestCertificate' action as soon as the request is made. By default, using this parameter causes email to be sent to all top-level domains you specified in the certificate request. The ARN must be of the form:  @arn:aws:acm:us-east-1:123456789012:certificate/12345678-1234-1234-1234-123456789012@
--
-- * 'rveDomain' - The fully qualified domain name (FQDN) of the certificate that needs to be validated.
--
-- * 'rveValidationDomain' - The base validation domain that will act as the suffix of the email addresses that are used to send the emails. This must be the same as the @Domain@ value or a superdomain of the @Domain@ value. For example, if you requested a certificate for @site.subdomain.example.com@ and specify a __ValidationDomain__ of @subdomain.example.com@ , ACM sends email to the domain registrant, technical contact, and administrative contact in WHOIS and the following five addresses:     * admin@subdomain.example.com     * administrator@subdomain.example.com     * hostmaster@subdomain.example.com     * postmaster@subdomain.example.com     * webmaster@subdomain.example.com
resendValidationEmail
    :: Text -- ^ 'rveCertificateARN'
    -> Text -- ^ 'rveDomain'
    -> Text -- ^ 'rveValidationDomain'
    -> ResendValidationEmail
resendValidationEmail pCertificateARN_ pDomain_ pValidationDomain_ =
  ResendValidationEmail'
    { _rveCertificateARN = pCertificateARN_
    , _rveDomain = pDomain_
    , _rveValidationDomain = pValidationDomain_
    }


-- | String that contains the ARN of the requested certificate. The certificate ARN is generated and returned by the 'RequestCertificate' action as soon as the request is made. By default, using this parameter causes email to be sent to all top-level domains you specified in the certificate request. The ARN must be of the form:  @arn:aws:acm:us-east-1:123456789012:certificate/12345678-1234-1234-1234-123456789012@
rveCertificateARN :: Lens' ResendValidationEmail Text
rveCertificateARN = lens _rveCertificateARN (\ s a -> s{_rveCertificateARN = a})

-- | The fully qualified domain name (FQDN) of the certificate that needs to be validated.
rveDomain :: Lens' ResendValidationEmail Text
rveDomain = lens _rveDomain (\ s a -> s{_rveDomain = a})

-- | The base validation domain that will act as the suffix of the email addresses that are used to send the emails. This must be the same as the @Domain@ value or a superdomain of the @Domain@ value. For example, if you requested a certificate for @site.subdomain.example.com@ and specify a __ValidationDomain__ of @subdomain.example.com@ , ACM sends email to the domain registrant, technical contact, and administrative contact in WHOIS and the following five addresses:     * admin@subdomain.example.com     * administrator@subdomain.example.com     * hostmaster@subdomain.example.com     * postmaster@subdomain.example.com     * webmaster@subdomain.example.com
rveValidationDomain :: Lens' ResendValidationEmail Text
rveValidationDomain = lens _rveValidationDomain (\ s a -> s{_rveValidationDomain = a})

instance AWSRequest ResendValidationEmail where
        type Rs ResendValidationEmail =
             ResendValidationEmailResponse
        request = postJSON certificateManager
        response = receiveNull ResendValidationEmailResponse'

instance Hashable ResendValidationEmail where

instance NFData ResendValidationEmail where

instance ToHeaders ResendValidationEmail where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CertificateManager.ResendValidationEmail" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ResendValidationEmail where
        toJSON ResendValidationEmail'{..}
          = object
              (catMaybes
                 [Just ("CertificateArn" .= _rveCertificateARN),
                  Just ("Domain" .= _rveDomain),
                  Just ("ValidationDomain" .= _rveValidationDomain)])

instance ToPath ResendValidationEmail where
        toPath = const "/"

instance ToQuery ResendValidationEmail where
        toQuery = const mempty

-- | /See:/ 'resendValidationEmailResponse' smart constructor.
data ResendValidationEmailResponse =
  ResendValidationEmailResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResendValidationEmailResponse' with the minimum fields required to make a request.
--
resendValidationEmailResponse
    :: ResendValidationEmailResponse
resendValidationEmailResponse = ResendValidationEmailResponse'


instance NFData ResendValidationEmailResponse where
