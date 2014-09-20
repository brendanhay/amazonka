{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB.SetLoadBalancerListenerSSLCertificate
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets the certificate that terminates the specified listener's SSL
-- connections. The specified certificate replaces any prior certificate that
-- was used on the same load balancer and port. For more information on
-- updating your SSL certificate, see Updating an SSL Certificate for a Load
-- Balancer in the Elastic Load Balancing Developer Guide.
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=MyInternalLoadBalancer
-- &SSLCertificateId=arn:aws:iam::123456789012:server-certificate/testcert
-- &LoadBalancerPort=443 &Version=2012-06-01
-- &Action=SetLoadBalancerListenerSSLCertificate &AUTHPARAMS
-- 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE.
module Network.AWS.ELB.SetLoadBalancerListenerSSLCertificate
    (
    -- * Request
      SetLoadBalancerListenerSSLCertificate
    -- ** Request constructor
    , setLoadBalancerListenerSSLCertificate
    -- ** Request lenses
    , slblsslcLoadBalancerName
    , slblsslcLoadBalancerPort
    , slblsslcSSLCertificateId

    -- * Response
    , SetLoadBalancerListenerSSLCertificateResponse
    -- ** Response constructor
    , setLoadBalancerListenerSSLCertificateResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.Types
import Network.AWS.Prelude

-- | The input for the SetLoadBalancerListenerSSLCertificate action.
data SetLoadBalancerListenerSSLCertificate = SetLoadBalancerListenerSSLCertificate
    { _slblsslcLoadBalancerName :: Text
    , _slblsslcLoadBalancerPort :: !Integer
    , _slblsslcSSLCertificateId :: Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SetLoadBalancerListenerSSLCertificate' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LoadBalancerName ::@ @Text@
--
-- * @LoadBalancerPort ::@ @Integer@
--
-- * @SSLCertificateId ::@ @Text@
--
setLoadBalancerListenerSSLCertificate :: Text -- ^ 'slblsslcLoadBalancerName'
                                      -> Integer -- ^ 'slblsslcLoadBalancerPort'
                                      -> Text -- ^ 'slblsslcSSLCertificateId'
                                      -> SetLoadBalancerListenerSSLCertificate
setLoadBalancerListenerSSLCertificate p1 p2 p3 = SetLoadBalancerListenerSSLCertificate
    { _slblsslcLoadBalancerName = p1
    , _slblsslcLoadBalancerPort = p2
    , _slblsslcSSLCertificateId = p3
    }

-- | The name of the load balancer.
slblsslcLoadBalancerName :: Lens' SetLoadBalancerListenerSSLCertificate Text
slblsslcLoadBalancerName =
    lens _slblsslcLoadBalancerName
         (\s a -> s { _slblsslcLoadBalancerName = a })

-- | The port that uses the specified SSL certificate.
slblsslcLoadBalancerPort :: Lens' SetLoadBalancerListenerSSLCertificate Integer
slblsslcLoadBalancerPort =
    lens _slblsslcLoadBalancerPort
         (\s a -> s { _slblsslcLoadBalancerPort = a })

-- | The Amazon Resource Number (ARN) of the SSL certificate chain to use. For
-- more information on SSL certificates, see Managing Server Certificates in
-- the AWS Identity and Access Management User Guide.
slblsslcSSLCertificateId :: Lens' SetLoadBalancerListenerSSLCertificate Text
slblsslcSSLCertificateId =
    lens _slblsslcSSLCertificateId
         (\s a -> s { _slblsslcSSLCertificateId = a })

instance ToQuery SetLoadBalancerListenerSSLCertificate where
    toQuery = genericQuery def

-- | The output for the SetLoadBalancerListenerSSLCertificate action.
data SetLoadBalancerListenerSSLCertificateResponse = SetLoadBalancerListenerSSLCertificateResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SetLoadBalancerListenerSSLCertificateResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
setLoadBalancerListenerSSLCertificateResponse :: SetLoadBalancerListenerSSLCertificateResponse
setLoadBalancerListenerSSLCertificateResponse = SetLoadBalancerListenerSSLCertificateResponse

instance AWSRequest SetLoadBalancerListenerSSLCertificate where
    type Sv SetLoadBalancerListenerSSLCertificate = ELB
    type Rs SetLoadBalancerListenerSSLCertificate = SetLoadBalancerListenerSSLCertificateResponse

    request = post "SetLoadBalancerListenerSSLCertificate"
    response _ = nullaryResponse SetLoadBalancerListenerSSLCertificateResponse
