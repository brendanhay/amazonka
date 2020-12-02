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
-- Module      : Network.AWS.SES.VerifyDomainIdentity
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a domain to the list of identities for your Amazon SES account and attempts to verify it. For more information about verifying domains, see <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Verifying Email Addresses and Domains> in the /Amazon SES Developer Guide./
--
--
-- You can execute this operation no more than once per second.
--
module Network.AWS.SES.VerifyDomainIdentity
    (
    -- * Creating a Request
      verifyDomainIdentity
    , VerifyDomainIdentity
    -- * Request Lenses
    , vdiDomain

    -- * Destructuring the Response
    , verifyDomainIdentityResponse
    , VerifyDomainIdentityResponse
    -- * Response Lenses
    , vdirsResponseStatus
    , vdirsVerificationToken
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to begin Amazon SES domain verification and to generate the TXT records that you must publish to the DNS server of your domain to complete the verification. For information about domain verification, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-domains.html Amazon SES Developer Guide> .
--
--
--
-- /See:/ 'verifyDomainIdentity' smart constructor.
newtype VerifyDomainIdentity = VerifyDomainIdentity'
  { _vdiDomain :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VerifyDomainIdentity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vdiDomain' - The domain to be verified.
verifyDomainIdentity
    :: Text -- ^ 'vdiDomain'
    -> VerifyDomainIdentity
verifyDomainIdentity pDomain_ = VerifyDomainIdentity' {_vdiDomain = pDomain_}


-- | The domain to be verified.
vdiDomain :: Lens' VerifyDomainIdentity Text
vdiDomain = lens _vdiDomain (\ s a -> s{_vdiDomain = a})

instance AWSRequest VerifyDomainIdentity where
        type Rs VerifyDomainIdentity =
             VerifyDomainIdentityResponse
        request = postQuery ses
        response
          = receiveXMLWrapper "VerifyDomainIdentityResult"
              (\ s h x ->
                 VerifyDomainIdentityResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "VerificationToken"))

instance Hashable VerifyDomainIdentity where

instance NFData VerifyDomainIdentity where

instance ToHeaders VerifyDomainIdentity where
        toHeaders = const mempty

instance ToPath VerifyDomainIdentity where
        toPath = const "/"

instance ToQuery VerifyDomainIdentity where
        toQuery VerifyDomainIdentity'{..}
          = mconcat
              ["Action" =: ("VerifyDomainIdentity" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "Domain" =: _vdiDomain]

-- | Returns a TXT record that you must publish to the DNS server of your domain to complete domain verification with Amazon SES.
--
--
--
-- /See:/ 'verifyDomainIdentityResponse' smart constructor.
data VerifyDomainIdentityResponse = VerifyDomainIdentityResponse'
  { _vdirsResponseStatus    :: !Int
  , _vdirsVerificationToken :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VerifyDomainIdentityResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vdirsResponseStatus' - -- | The response status code.
--
-- * 'vdirsVerificationToken' - A TXT record that you must place in the DNS settings of the domain to complete domain verification with Amazon SES. As Amazon SES searches for the TXT record, the domain's verification status is "Pending". When Amazon SES detects the record, the domain's verification status changes to "Success". If Amazon SES is unable to detect the record within 72 hours, the domain's verification status changes to "Failed." In that case, if you still want to verify the domain, you must restart the verification process from the beginning.
verifyDomainIdentityResponse
    :: Int -- ^ 'vdirsResponseStatus'
    -> Text -- ^ 'vdirsVerificationToken'
    -> VerifyDomainIdentityResponse
verifyDomainIdentityResponse pResponseStatus_ pVerificationToken_ =
  VerifyDomainIdentityResponse'
    { _vdirsResponseStatus = pResponseStatus_
    , _vdirsVerificationToken = pVerificationToken_
    }


-- | -- | The response status code.
vdirsResponseStatus :: Lens' VerifyDomainIdentityResponse Int
vdirsResponseStatus = lens _vdirsResponseStatus (\ s a -> s{_vdirsResponseStatus = a})

-- | A TXT record that you must place in the DNS settings of the domain to complete domain verification with Amazon SES. As Amazon SES searches for the TXT record, the domain's verification status is "Pending". When Amazon SES detects the record, the domain's verification status changes to "Success". If Amazon SES is unable to detect the record within 72 hours, the domain's verification status changes to "Failed." In that case, if you still want to verify the domain, you must restart the verification process from the beginning.
vdirsVerificationToken :: Lens' VerifyDomainIdentityResponse Text
vdirsVerificationToken = lens _vdirsVerificationToken (\ s a -> s{_vdirsVerificationToken = a})

instance NFData VerifyDomainIdentityResponse where
