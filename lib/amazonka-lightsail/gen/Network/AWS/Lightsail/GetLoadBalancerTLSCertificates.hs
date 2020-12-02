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
-- Module      : Network.AWS.Lightsail.GetLoadBalancerTLSCertificates
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the TLS certificates that are associated with the specified Lightsail load balancer.
--
--
-- TLS is just an updated, more secure version of Secure Socket Layer (SSL).
--
-- You can have a maximum of 2 certificates associated with a Lightsail load balancer. One is active and the other is inactive.
--
module Network.AWS.Lightsail.GetLoadBalancerTLSCertificates
    (
    -- * Creating a Request
      getLoadBalancerTLSCertificates
    , GetLoadBalancerTLSCertificates
    -- * Request Lenses
    , glbtcLoadBalancerName

    -- * Destructuring the Response
    , getLoadBalancerTLSCertificatesResponse
    , GetLoadBalancerTLSCertificatesResponse
    -- * Response Lenses
    , glbtcrsTlsCertificates
    , glbtcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getLoadBalancerTLSCertificates' smart constructor.
newtype GetLoadBalancerTLSCertificates = GetLoadBalancerTLSCertificates'
  { _glbtcLoadBalancerName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetLoadBalancerTLSCertificates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glbtcLoadBalancerName' - The name of the load balancer you associated with your SSL/TLS certificate.
getLoadBalancerTLSCertificates
    :: Text -- ^ 'glbtcLoadBalancerName'
    -> GetLoadBalancerTLSCertificates
getLoadBalancerTLSCertificates pLoadBalancerName_ =
  GetLoadBalancerTLSCertificates' {_glbtcLoadBalancerName = pLoadBalancerName_}


-- | The name of the load balancer you associated with your SSL/TLS certificate.
glbtcLoadBalancerName :: Lens' GetLoadBalancerTLSCertificates Text
glbtcLoadBalancerName = lens _glbtcLoadBalancerName (\ s a -> s{_glbtcLoadBalancerName = a})

instance AWSRequest GetLoadBalancerTLSCertificates
         where
        type Rs GetLoadBalancerTLSCertificates =
             GetLoadBalancerTLSCertificatesResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 GetLoadBalancerTLSCertificatesResponse' <$>
                   (x .?> "tlsCertificates" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable GetLoadBalancerTLSCertificates
         where

instance NFData GetLoadBalancerTLSCertificates where

instance ToHeaders GetLoadBalancerTLSCertificates
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.GetLoadBalancerTlsCertificates"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetLoadBalancerTLSCertificates where
        toJSON GetLoadBalancerTLSCertificates'{..}
          = object
              (catMaybes
                 [Just
                    ("loadBalancerName" .= _glbtcLoadBalancerName)])

instance ToPath GetLoadBalancerTLSCertificates where
        toPath = const "/"

instance ToQuery GetLoadBalancerTLSCertificates where
        toQuery = const mempty

-- | /See:/ 'getLoadBalancerTLSCertificatesResponse' smart constructor.
data GetLoadBalancerTLSCertificatesResponse = GetLoadBalancerTLSCertificatesResponse'
  { _glbtcrsTlsCertificates :: !(Maybe [LoadBalancerTLSCertificate])
  , _glbtcrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetLoadBalancerTLSCertificatesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glbtcrsTlsCertificates' - An array of LoadBalancerTlsCertificate objects describing your SSL/TLS certificates.
--
-- * 'glbtcrsResponseStatus' - -- | The response status code.
getLoadBalancerTLSCertificatesResponse
    :: Int -- ^ 'glbtcrsResponseStatus'
    -> GetLoadBalancerTLSCertificatesResponse
getLoadBalancerTLSCertificatesResponse pResponseStatus_ =
  GetLoadBalancerTLSCertificatesResponse'
    { _glbtcrsTlsCertificates = Nothing
    , _glbtcrsResponseStatus = pResponseStatus_
    }


-- | An array of LoadBalancerTlsCertificate objects describing your SSL/TLS certificates.
glbtcrsTlsCertificates :: Lens' GetLoadBalancerTLSCertificatesResponse [LoadBalancerTLSCertificate]
glbtcrsTlsCertificates = lens _glbtcrsTlsCertificates (\ s a -> s{_glbtcrsTlsCertificates = a}) . _Default . _Coerce

-- | -- | The response status code.
glbtcrsResponseStatus :: Lens' GetLoadBalancerTLSCertificatesResponse Int
glbtcrsResponseStatus = lens _glbtcrsResponseStatus (\ s a -> s{_glbtcrsResponseStatus = a})

instance NFData
           GetLoadBalancerTLSCertificatesResponse
         where
