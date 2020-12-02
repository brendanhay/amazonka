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
-- Module      : Network.AWS.Lightsail.AttachLoadBalancerTLSCertificate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a Transport Layer Security (TLS) certificate to your load balancer. TLS is just an updated, more secure version of Secure Socket Layer (SSL).
--
--
-- Once you create and validate your certificate, you can attach it to your load balancer. You can also use this API to rotate the certificates on your account. Use the @AttachLoadBalancerTlsCertificate@ operation with the non-attached certificate, and it will replace the existing one and become the attached certificate.
--
module Network.AWS.Lightsail.AttachLoadBalancerTLSCertificate
    (
    -- * Creating a Request
      attachLoadBalancerTLSCertificate
    , AttachLoadBalancerTLSCertificate
    -- * Request Lenses
    , albtcLoadBalancerName
    , albtcCertificateName

    -- * Destructuring the Response
    , attachLoadBalancerTLSCertificateResponse
    , AttachLoadBalancerTLSCertificateResponse
    -- * Response Lenses
    , albtcrsOperations
    , albtcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'attachLoadBalancerTLSCertificate' smart constructor.
data AttachLoadBalancerTLSCertificate = AttachLoadBalancerTLSCertificate'
  { _albtcLoadBalancerName :: !Text
  , _albtcCertificateName  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachLoadBalancerTLSCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'albtcLoadBalancerName' - The name of the load balancer to which you want to associate the SSL/TLS certificate.
--
-- * 'albtcCertificateName' - The name of your SSL/TLS certificate.
attachLoadBalancerTLSCertificate
    :: Text -- ^ 'albtcLoadBalancerName'
    -> Text -- ^ 'albtcCertificateName'
    -> AttachLoadBalancerTLSCertificate
attachLoadBalancerTLSCertificate pLoadBalancerName_ pCertificateName_ =
  AttachLoadBalancerTLSCertificate'
    { _albtcLoadBalancerName = pLoadBalancerName_
    , _albtcCertificateName = pCertificateName_
    }


-- | The name of the load balancer to which you want to associate the SSL/TLS certificate.
albtcLoadBalancerName :: Lens' AttachLoadBalancerTLSCertificate Text
albtcLoadBalancerName = lens _albtcLoadBalancerName (\ s a -> s{_albtcLoadBalancerName = a})

-- | The name of your SSL/TLS certificate.
albtcCertificateName :: Lens' AttachLoadBalancerTLSCertificate Text
albtcCertificateName = lens _albtcCertificateName (\ s a -> s{_albtcCertificateName = a})

instance AWSRequest AttachLoadBalancerTLSCertificate
         where
        type Rs AttachLoadBalancerTLSCertificate =
             AttachLoadBalancerTLSCertificateResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 AttachLoadBalancerTLSCertificateResponse' <$>
                   (x .?> "operations" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable AttachLoadBalancerTLSCertificate
         where

instance NFData AttachLoadBalancerTLSCertificate
         where

instance ToHeaders AttachLoadBalancerTLSCertificate
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.AttachLoadBalancerTlsCertificate"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AttachLoadBalancerTLSCertificate
         where
        toJSON AttachLoadBalancerTLSCertificate'{..}
          = object
              (catMaybes
                 [Just ("loadBalancerName" .= _albtcLoadBalancerName),
                  Just ("certificateName" .= _albtcCertificateName)])

instance ToPath AttachLoadBalancerTLSCertificate
         where
        toPath = const "/"

instance ToQuery AttachLoadBalancerTLSCertificate
         where
        toQuery = const mempty

-- | /See:/ 'attachLoadBalancerTLSCertificateResponse' smart constructor.
data AttachLoadBalancerTLSCertificateResponse = AttachLoadBalancerTLSCertificateResponse'
  { _albtcrsOperations     :: !(Maybe [Operation])
  , _albtcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachLoadBalancerTLSCertificateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'albtcrsOperations' - An object representing the API operations. These SSL/TLS certificates are only usable by Lightsail load balancers. You can't get the certificate and use it for another purpose.
--
-- * 'albtcrsResponseStatus' - -- | The response status code.
attachLoadBalancerTLSCertificateResponse
    :: Int -- ^ 'albtcrsResponseStatus'
    -> AttachLoadBalancerTLSCertificateResponse
attachLoadBalancerTLSCertificateResponse pResponseStatus_ =
  AttachLoadBalancerTLSCertificateResponse'
    {_albtcrsOperations = Nothing, _albtcrsResponseStatus = pResponseStatus_}


-- | An object representing the API operations. These SSL/TLS certificates are only usable by Lightsail load balancers. You can't get the certificate and use it for another purpose.
albtcrsOperations :: Lens' AttachLoadBalancerTLSCertificateResponse [Operation]
albtcrsOperations = lens _albtcrsOperations (\ s a -> s{_albtcrsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
albtcrsResponseStatus :: Lens' AttachLoadBalancerTLSCertificateResponse Int
albtcrsResponseStatus = lens _albtcrsResponseStatus (\ s a -> s{_albtcrsResponseStatus = a})

instance NFData
           AttachLoadBalancerTLSCertificateResponse
         where
