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
-- Module      : Network.AWS.Lightsail.CreateLoadBalancerTLSCertificate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Lightsail load balancer TLS certificate.
--
--
-- TLS is just an updated, more secure version of Secure Socket Layer (SSL).
--
module Network.AWS.Lightsail.CreateLoadBalancerTLSCertificate
    (
    -- * Creating a Request
      createLoadBalancerTLSCertificate
    , CreateLoadBalancerTLSCertificate
    -- * Request Lenses
    , clbtcCertificateAlternativeNames
    , clbtcLoadBalancerName
    , clbtcCertificateName
    , clbtcCertificateDomainName

    -- * Destructuring the Response
    , createLoadBalancerTLSCertificateResponse
    , CreateLoadBalancerTLSCertificateResponse
    -- * Response Lenses
    , clbtcrsOperations
    , clbtcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createLoadBalancerTLSCertificate' smart constructor.
data CreateLoadBalancerTLSCertificate = CreateLoadBalancerTLSCertificate'
  { _clbtcCertificateAlternativeNames :: !(Maybe [Text])
  , _clbtcLoadBalancerName            :: !Text
  , _clbtcCertificateName             :: !Text
  , _clbtcCertificateDomainName       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLoadBalancerTLSCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clbtcCertificateAlternativeNames' - An array of strings listing alternative domains and subdomains for your SSL/TLS certificate. Lightsail will de-dupe the names for you. You can have a maximum of 9 alternative names (in addition to the 1 primary domain). We do not support wildcards (e.g., @*.example.com@ ).
--
-- * 'clbtcLoadBalancerName' - The load balancer name where you want to create the SSL/TLS certificate.
--
-- * 'clbtcCertificateName' - The SSL/TLS certificate name. You can have up to 10 certificates in your account at one time. Each Lightsail load balancer can have up to 2 certificates associated with it at one time. There is also an overall limit to the number of certificates that can be issue in a 365-day period. For more information, see <http://docs.aws.amazon.com/acm/latest/userguide/acm-limits.html Limits> .
--
-- * 'clbtcCertificateDomainName' - The domain name (e.g., @example.com@ ) for your SSL/TLS certificate.
createLoadBalancerTLSCertificate
    :: Text -- ^ 'clbtcLoadBalancerName'
    -> Text -- ^ 'clbtcCertificateName'
    -> Text -- ^ 'clbtcCertificateDomainName'
    -> CreateLoadBalancerTLSCertificate
createLoadBalancerTLSCertificate pLoadBalancerName_ pCertificateName_ pCertificateDomainName_ =
  CreateLoadBalancerTLSCertificate'
    { _clbtcCertificateAlternativeNames = Nothing
    , _clbtcLoadBalancerName = pLoadBalancerName_
    , _clbtcCertificateName = pCertificateName_
    , _clbtcCertificateDomainName = pCertificateDomainName_
    }


-- | An array of strings listing alternative domains and subdomains for your SSL/TLS certificate. Lightsail will de-dupe the names for you. You can have a maximum of 9 alternative names (in addition to the 1 primary domain). We do not support wildcards (e.g., @*.example.com@ ).
clbtcCertificateAlternativeNames :: Lens' CreateLoadBalancerTLSCertificate [Text]
clbtcCertificateAlternativeNames = lens _clbtcCertificateAlternativeNames (\ s a -> s{_clbtcCertificateAlternativeNames = a}) . _Default . _Coerce

-- | The load balancer name where you want to create the SSL/TLS certificate.
clbtcLoadBalancerName :: Lens' CreateLoadBalancerTLSCertificate Text
clbtcLoadBalancerName = lens _clbtcLoadBalancerName (\ s a -> s{_clbtcLoadBalancerName = a})

-- | The SSL/TLS certificate name. You can have up to 10 certificates in your account at one time. Each Lightsail load balancer can have up to 2 certificates associated with it at one time. There is also an overall limit to the number of certificates that can be issue in a 365-day period. For more information, see <http://docs.aws.amazon.com/acm/latest/userguide/acm-limits.html Limits> .
clbtcCertificateName :: Lens' CreateLoadBalancerTLSCertificate Text
clbtcCertificateName = lens _clbtcCertificateName (\ s a -> s{_clbtcCertificateName = a})

-- | The domain name (e.g., @example.com@ ) for your SSL/TLS certificate.
clbtcCertificateDomainName :: Lens' CreateLoadBalancerTLSCertificate Text
clbtcCertificateDomainName = lens _clbtcCertificateDomainName (\ s a -> s{_clbtcCertificateDomainName = a})

instance AWSRequest CreateLoadBalancerTLSCertificate
         where
        type Rs CreateLoadBalancerTLSCertificate =
             CreateLoadBalancerTLSCertificateResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 CreateLoadBalancerTLSCertificateResponse' <$>
                   (x .?> "operations" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable CreateLoadBalancerTLSCertificate
         where

instance NFData CreateLoadBalancerTLSCertificate
         where

instance ToHeaders CreateLoadBalancerTLSCertificate
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.CreateLoadBalancerTlsCertificate"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateLoadBalancerTLSCertificate
         where
        toJSON CreateLoadBalancerTLSCertificate'{..}
          = object
              (catMaybes
                 [("certificateAlternativeNames" .=) <$>
                    _clbtcCertificateAlternativeNames,
                  Just ("loadBalancerName" .= _clbtcLoadBalancerName),
                  Just ("certificateName" .= _clbtcCertificateName),
                  Just
                    ("certificateDomainName" .=
                       _clbtcCertificateDomainName)])

instance ToPath CreateLoadBalancerTLSCertificate
         where
        toPath = const "/"

instance ToQuery CreateLoadBalancerTLSCertificate
         where
        toQuery = const mempty

-- | /See:/ 'createLoadBalancerTLSCertificateResponse' smart constructor.
data CreateLoadBalancerTLSCertificateResponse = CreateLoadBalancerTLSCertificateResponse'
  { _clbtcrsOperations     :: !(Maybe [Operation])
  , _clbtcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLoadBalancerTLSCertificateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clbtcrsOperations' - An object containing information about the API operations.
--
-- * 'clbtcrsResponseStatus' - -- | The response status code.
createLoadBalancerTLSCertificateResponse
    :: Int -- ^ 'clbtcrsResponseStatus'
    -> CreateLoadBalancerTLSCertificateResponse
createLoadBalancerTLSCertificateResponse pResponseStatus_ =
  CreateLoadBalancerTLSCertificateResponse'
    {_clbtcrsOperations = Nothing, _clbtcrsResponseStatus = pResponseStatus_}


-- | An object containing information about the API operations.
clbtcrsOperations :: Lens' CreateLoadBalancerTLSCertificateResponse [Operation]
clbtcrsOperations = lens _clbtcrsOperations (\ s a -> s{_clbtcrsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
clbtcrsResponseStatus :: Lens' CreateLoadBalancerTLSCertificateResponse Int
clbtcrsResponseStatus = lens _clbtcrsResponseStatus (\ s a -> s{_clbtcrsResponseStatus = a})

instance NFData
           CreateLoadBalancerTLSCertificateResponse
         where
