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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the certificate that terminates the specified listener\'s SSL
-- connections. The specified certificate replaces any prior certificate
-- that was used on the same load balancer and port.
--
-- For more information about updating your SSL certificate, see
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/US_UpdatingLoadBalancerSSL.html Updating an SSL Certificate for a Load Balancer>
-- in the /Elastic Load Balancing Developer Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_SetLoadBalancerListenerSSLCertificate.html AWS API Reference> for SetLoadBalancerListenerSSLCertificate.
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
    , slblscrsStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.ELB.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'setLoadBalancerListenerSSLCertificate' smart constructor.
data SetLoadBalancerListenerSSLCertificate = SetLoadBalancerListenerSSLCertificate'
    { _slblscLoadBalancerName :: !Text
    , _slblscLoadBalancerPort :: !Int
    , _slblscSSLCertificateId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SetLoadBalancerListenerSSLCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slblscLoadBalancerName'
--
-- * 'slblscLoadBalancerPort'
--
-- * 'slblscSSLCertificateId'
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
slblscLoadBalancerName = lens _slblscLoadBalancerName (\ s a -> s{_slblscLoadBalancerName = a});

-- | The port that uses the specified SSL certificate.
slblscLoadBalancerPort :: Lens' SetLoadBalancerListenerSSLCertificate Int
slblscLoadBalancerPort = lens _slblscLoadBalancerPort (\ s a -> s{_slblscLoadBalancerPort = a});

-- | The Amazon Resource Name (ARN) of the SSL certificate.
slblscSSLCertificateId :: Lens' SetLoadBalancerListenerSSLCertificate Text
slblscSSLCertificateId = lens _slblscSSLCertificateId (\ s a -> s{_slblscSSLCertificateId = a});

instance AWSRequest
         SetLoadBalancerListenerSSLCertificate where
        type Rs SetLoadBalancerListenerSSLCertificate =
             SetLoadBalancerListenerSSLCertificateResponse
        request = postQuery eLB
        response
          = receiveXMLWrapper
              "SetLoadBalancerListenerSSLCertificateResult"
              (\ s h x ->
                 SetLoadBalancerListenerSSLCertificateResponse' <$>
                   (pure (fromEnum s)))

instance ToHeaders
         SetLoadBalancerListenerSSLCertificate where
        toHeaders = const mempty

instance ToPath SetLoadBalancerListenerSSLCertificate
         where
        toPath = const "/"

instance ToQuery
         SetLoadBalancerListenerSSLCertificate where
        toQuery SetLoadBalancerListenerSSLCertificate'{..}
          = mconcat
              ["Action" =:
                 ("SetLoadBalancerListenerSSLCertificate" ::
                    ByteString),
               "Version" =: ("2012-06-01" :: ByteString),
               "LoadBalancerName" =: _slblscLoadBalancerName,
               "LoadBalancerPort" =: _slblscLoadBalancerPort,
               "SSLCertificateId" =: _slblscSSLCertificateId]

-- | /See:/ 'setLoadBalancerListenerSSLCertificateResponse' smart constructor.
newtype SetLoadBalancerListenerSSLCertificateResponse = SetLoadBalancerListenerSSLCertificateResponse'
    { _slblscrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SetLoadBalancerListenerSSLCertificateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slblscrsStatus'
setLoadBalancerListenerSSLCertificateResponse
    :: Int -- ^ 'slblscrsStatus'
    -> SetLoadBalancerListenerSSLCertificateResponse
setLoadBalancerListenerSSLCertificateResponse pStatus_ =
    SetLoadBalancerListenerSSLCertificateResponse'
    { _slblscrsStatus = pStatus_
    }

-- | The response status code.
slblscrsStatus :: Lens' SetLoadBalancerListenerSSLCertificateResponse Int
slblscrsStatus = lens _slblscrsStatus (\ s a -> s{_slblscrsStatus = a});
