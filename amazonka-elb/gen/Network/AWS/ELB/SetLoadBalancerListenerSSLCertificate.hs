{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.SetLoadBalancerListenerSSLCertificate
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_SetLoadBalancerListenerSSLCertificate.html>
module Network.AWS.ELB.SetLoadBalancerListenerSSLCertificate
    (
    -- * Request
      SetLoadBalancerListenerSSLCertificate
    -- ** Request constructor
    , setLoadBalancerListenerSSLCertificate
    -- ** Request lenses
    , slblscrqLoadBalancerName
    , slblscrqLoadBalancerPort
    , slblscrqSSLCertificateId

    -- * Response
    , SetLoadBalancerListenerSSLCertificateResponse
    -- ** Response constructor
    , setLoadBalancerListenerSSLCertificateResponse
    -- ** Response lenses
    , slblscrsStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'setLoadBalancerListenerSSLCertificate' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'slblscrqLoadBalancerName'
--
-- * 'slblscrqLoadBalancerPort'
--
-- * 'slblscrqSSLCertificateId'
data SetLoadBalancerListenerSSLCertificate = SetLoadBalancerListenerSSLCertificate'
    { _slblscrqLoadBalancerName :: !Text
    , _slblscrqLoadBalancerPort :: !Int
    , _slblscrqSSLCertificateId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetLoadBalancerListenerSSLCertificate' smart constructor.
setLoadBalancerListenerSSLCertificate :: Text -> Int -> Text -> SetLoadBalancerListenerSSLCertificate
setLoadBalancerListenerSSLCertificate pLoadBalancerName pLoadBalancerPort pSSLCertificateId =
    SetLoadBalancerListenerSSLCertificate'
    { _slblscrqLoadBalancerName = pLoadBalancerName
    , _slblscrqLoadBalancerPort = pLoadBalancerPort
    , _slblscrqSSLCertificateId = pSSLCertificateId
    }

-- | The name of the load balancer.
slblscrqLoadBalancerName :: Lens' SetLoadBalancerListenerSSLCertificate Text
slblscrqLoadBalancerName = lens _slblscrqLoadBalancerName (\ s a -> s{_slblscrqLoadBalancerName = a});

-- | The port that uses the specified SSL certificate.
slblscrqLoadBalancerPort :: Lens' SetLoadBalancerListenerSSLCertificate Int
slblscrqLoadBalancerPort = lens _slblscrqLoadBalancerPort (\ s a -> s{_slblscrqLoadBalancerPort = a});

-- | The Amazon Resource Name (ARN) of the SSL certificate.
slblscrqSSLCertificateId :: Lens' SetLoadBalancerListenerSSLCertificate Text
slblscrqSSLCertificateId = lens _slblscrqSSLCertificateId (\ s a -> s{_slblscrqSSLCertificateId = a});

instance AWSRequest
         SetLoadBalancerListenerSSLCertificate where
        type Sv SetLoadBalancerListenerSSLCertificate = ELB
        type Rs SetLoadBalancerListenerSSLCertificate =
             SetLoadBalancerListenerSSLCertificateResponse
        request = post
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
               "LoadBalancerName" =: _slblscrqLoadBalancerName,
               "LoadBalancerPort" =: _slblscrqLoadBalancerPort,
               "SSLCertificateId" =: _slblscrqSSLCertificateId]

-- | /See:/ 'setLoadBalancerListenerSSLCertificateResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'slblscrsStatus'
newtype SetLoadBalancerListenerSSLCertificateResponse = SetLoadBalancerListenerSSLCertificateResponse'
    { _slblscrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetLoadBalancerListenerSSLCertificateResponse' smart constructor.
setLoadBalancerListenerSSLCertificateResponse :: Int -> SetLoadBalancerListenerSSLCertificateResponse
setLoadBalancerListenerSSLCertificateResponse pStatus =
    SetLoadBalancerListenerSSLCertificateResponse'
    { _slblscrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
slblscrsStatus :: Lens' SetLoadBalancerListenerSSLCertificateResponse Int
slblscrsStatus = lens _slblscrsStatus (\ s a -> s{_slblscrsStatus = a});
