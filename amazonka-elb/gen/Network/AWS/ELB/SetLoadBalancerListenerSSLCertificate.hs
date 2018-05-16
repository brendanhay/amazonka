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
-- Module      : Network.AWS.ELB.SetLoadBalancerListenerSSLCertificate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the certificate that terminates the specified listener's SSL connections. The specified certificate replaces any prior certificate that was used on the same load balancer and port.
--
--
-- For more information about updating your SSL certificate, see <http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-update-ssl-cert.html Replace the SSL Certificate for Your Load Balancer> in the /Classic Load Balancer Guide/ .
--
module Network.AWS.ELB.SetLoadBalancerListenerSSLCertificate
    (
    -- * Creating a Request
      setLoadBalancerListenerSSLCertificate
    , SetLoadBalancerListenerSSLCertificate
    -- * Request Lenses
    , slblscLoadBalancerName
    , slblscLoadBalancerPort
    , slblscSSLCertificateId

    -- * Destructuring the Response
    , setLoadBalancerListenerSSLCertificateResponse
    , SetLoadBalancerListenerSSLCertificateResponse
    -- * Response Lenses
    , slblscrsResponseStatus
    ) where

import Network.AWS.ELB.Types
import Network.AWS.ELB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for SetLoadBalancerListenerSSLCertificate.
--
--
--
-- /See:/ 'setLoadBalancerListenerSSLCertificate' smart constructor.
data SetLoadBalancerListenerSSLCertificate = SetLoadBalancerListenerSSLCertificate'
  { _slblscLoadBalancerName :: !Text
  , _slblscLoadBalancerPort :: !Int
  , _slblscSSLCertificateId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetLoadBalancerListenerSSLCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slblscLoadBalancerName' - The name of the load balancer.
--
-- * 'slblscLoadBalancerPort' - The port that uses the specified SSL certificate.
--
-- * 'slblscSSLCertificateId' - The Amazon Resource Name (ARN) of the SSL certificate.
setLoadBalancerListenerSSLCertificate
    :: Text -- ^ 'slblscLoadBalancerName'
    -> Int -- ^ 'slblscLoadBalancerPort'
    -> Text -- ^ 'slblscSSLCertificateId'
    -> SetLoadBalancerListenerSSLCertificate
setLoadBalancerListenerSSLCertificate pLoadBalancerName_ pLoadBalancerPort_ pSSLCertificateId_ =
  SetLoadBalancerListenerSSLCertificate'
    { _slblscLoadBalancerName = pLoadBalancerName_
    , _slblscLoadBalancerPort = pLoadBalancerPort_
    , _slblscSSLCertificateId = pSSLCertificateId_
    }


-- | The name of the load balancer.
slblscLoadBalancerName :: Lens' SetLoadBalancerListenerSSLCertificate Text
slblscLoadBalancerName = lens _slblscLoadBalancerName (\ s a -> s{_slblscLoadBalancerName = a})

-- | The port that uses the specified SSL certificate.
slblscLoadBalancerPort :: Lens' SetLoadBalancerListenerSSLCertificate Int
slblscLoadBalancerPort = lens _slblscLoadBalancerPort (\ s a -> s{_slblscLoadBalancerPort = a})

-- | The Amazon Resource Name (ARN) of the SSL certificate.
slblscSSLCertificateId :: Lens' SetLoadBalancerListenerSSLCertificate Text
slblscSSLCertificateId = lens _slblscSSLCertificateId (\ s a -> s{_slblscSSLCertificateId = a})

instance AWSRequest
           SetLoadBalancerListenerSSLCertificate
         where
        type Rs SetLoadBalancerListenerSSLCertificate =
             SetLoadBalancerListenerSSLCertificateResponse
        request = postQuery elb
        response
          = receiveXMLWrapper
              "SetLoadBalancerListenerSSLCertificateResult"
              (\ s h x ->
                 SetLoadBalancerListenerSSLCertificateResponse' <$>
                   (pure (fromEnum s)))

instance Hashable
           SetLoadBalancerListenerSSLCertificate
         where

instance NFData SetLoadBalancerListenerSSLCertificate
         where

instance ToHeaders
           SetLoadBalancerListenerSSLCertificate
         where
        toHeaders = const mempty

instance ToPath SetLoadBalancerListenerSSLCertificate
         where
        toPath = const "/"

instance ToQuery
           SetLoadBalancerListenerSSLCertificate
         where
        toQuery SetLoadBalancerListenerSSLCertificate'{..}
          = mconcat
              ["Action" =:
                 ("SetLoadBalancerListenerSSLCertificate" ::
                    ByteString),
               "Version" =: ("2012-06-01" :: ByteString),
               "LoadBalancerName" =: _slblscLoadBalancerName,
               "LoadBalancerPort" =: _slblscLoadBalancerPort,
               "SSLCertificateId" =: _slblscSSLCertificateId]

-- | Contains the output of SetLoadBalancerListenerSSLCertificate.
--
--
--
-- /See:/ 'setLoadBalancerListenerSSLCertificateResponse' smart constructor.
newtype SetLoadBalancerListenerSSLCertificateResponse = SetLoadBalancerListenerSSLCertificateResponse'
  { _slblscrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetLoadBalancerListenerSSLCertificateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slblscrsResponseStatus' - -- | The response status code.
setLoadBalancerListenerSSLCertificateResponse
    :: Int -- ^ 'slblscrsResponseStatus'
    -> SetLoadBalancerListenerSSLCertificateResponse
setLoadBalancerListenerSSLCertificateResponse pResponseStatus_ =
  SetLoadBalancerListenerSSLCertificateResponse'
    {_slblscrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
slblscrsResponseStatus :: Lens' SetLoadBalancerListenerSSLCertificateResponse Int
slblscrsResponseStatus = lens _slblscrsResponseStatus (\ s a -> s{_slblscrsResponseStatus = a})

instance NFData
           SetLoadBalancerListenerSSLCertificateResponse
         where
