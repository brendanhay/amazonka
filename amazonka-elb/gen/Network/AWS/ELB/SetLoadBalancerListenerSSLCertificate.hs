{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.ELB.SetLoadBalancerListenerSSLCertificate
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Sets the certificate that terminates the specified listener\'s SSL
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
    , slblscLoadBalancerName
    , slblscLoadBalancerPort
    , slblscSSLCertificateId

    -- * Response
    , SetLoadBalancerListenerSSLCertificateResponse
    -- ** Response constructor
    , setLoadBalancerListenerSSLCertificateResponse
    -- ** Response lenses
    , slblscrStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'setLoadBalancerListenerSSLCertificate' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'slblscLoadBalancerName'
--
-- * 'slblscLoadBalancerPort'
--
-- * 'slblscSSLCertificateId'
data SetLoadBalancerListenerSSLCertificate = SetLoadBalancerListenerSSLCertificate'
    { _slblscLoadBalancerName :: !Text
    , _slblscLoadBalancerPort :: !Int
    , _slblscSSLCertificateId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetLoadBalancerListenerSSLCertificate' smart constructor.
setLoadBalancerListenerSSLCertificate :: Text -> Int -> Text -> SetLoadBalancerListenerSSLCertificate
setLoadBalancerListenerSSLCertificate pLoadBalancerName pLoadBalancerPort pSSLCertificateId =
    SetLoadBalancerListenerSSLCertificate'
    { _slblscLoadBalancerName = pLoadBalancerName
    , _slblscLoadBalancerPort = pLoadBalancerPort
    , _slblscSSLCertificateId = pSSLCertificateId
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
               "LoadBalancerName" =: _slblscLoadBalancerName,
               "LoadBalancerPort" =: _slblscLoadBalancerPort,
               "SSLCertificateId" =: _slblscSSLCertificateId]

-- | /See:/ 'setLoadBalancerListenerSSLCertificateResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'slblscrStatus'
newtype SetLoadBalancerListenerSSLCertificateResponse = SetLoadBalancerListenerSSLCertificateResponse'
    { _slblscrStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetLoadBalancerListenerSSLCertificateResponse' smart constructor.
setLoadBalancerListenerSSLCertificateResponse :: Int -> SetLoadBalancerListenerSSLCertificateResponse
setLoadBalancerListenerSSLCertificateResponse pStatus =
    SetLoadBalancerListenerSSLCertificateResponse'
    { _slblscrStatus = pStatus
    }

-- | FIXME: Undocumented member.
slblscrStatus :: Lens' SetLoadBalancerListenerSSLCertificateResponse Int
slblscrStatus = lens _slblscrStatus (\ s a -> s{_slblscrStatus = a});
