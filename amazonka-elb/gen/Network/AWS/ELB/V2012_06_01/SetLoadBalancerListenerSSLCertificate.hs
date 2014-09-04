{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB.V2012_06_01.SetLoadBalancerListenerSSLCertificate
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
module Network.AWS.ELB.V2012_06_01.SetLoadBalancerListenerSSLCertificate
    (
    -- * Request
      SetLoadBalancerListenerSSLCertificate
    -- ** Request constructor
    , mkSetLoadBalancerListenerSSLCertificateInput
    -- ** Request lenses
    , slblsslciLoadBalancerName
    , slblsslciLoadBalancerPort
    , slblsslciSSLCertificateId

    -- * Response
    , SetLoadBalancerListenerSSLCertificateResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.V2012_06_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SetLoadBalancerListenerSSLCertificate' request.
mkSetLoadBalancerListenerSSLCertificateInput :: Text -- ^ 'slblsslciLoadBalancerName'
                                             -> Integer -- ^ 'slblsslciLoadBalancerPort'
                                             -> Text -- ^ 'slblsslciSSLCertificateId'
                                             -> SetLoadBalancerListenerSSLCertificate
mkSetLoadBalancerListenerSSLCertificateInput p1 p2 p3 = SetLoadBalancerListenerSSLCertificate
    { _slblsslciLoadBalancerName = p1
    , _slblsslciLoadBalancerPort = p2
    , _slblsslciSSLCertificateId = p3
    }
{-# INLINE mkSetLoadBalancerListenerSSLCertificateInput #-}

data SetLoadBalancerListenerSSLCertificate = SetLoadBalancerListenerSSLCertificate
    { _slblsslciLoadBalancerName :: Text
      -- ^ The name of the load balancer.
    , _slblsslciLoadBalancerPort :: Integer
      -- ^ The port that uses the specified SSL certificate.
    , _slblsslciSSLCertificateId :: Text
      -- ^ The Amazon Resource Number (ARN) of the SSL certificate chain to
      -- use. For more information on SSL certificates, see Managing
      -- Server Certificates in the AWS Identity and Access Management
      -- User Guide.
    } deriving (Show, Generic)

-- | The name of the load balancer.
slblsslciLoadBalancerName :: Lens' SetLoadBalancerListenerSSLCertificate (Text)
slblsslciLoadBalancerName = lens _slblsslciLoadBalancerName (\s a -> s { _slblsslciLoadBalancerName = a })
{-# INLINE slblsslciLoadBalancerName #-}

-- | The port that uses the specified SSL certificate.
slblsslciLoadBalancerPort :: Lens' SetLoadBalancerListenerSSLCertificate (Integer)
slblsslciLoadBalancerPort = lens _slblsslciLoadBalancerPort (\s a -> s { _slblsslciLoadBalancerPort = a })
{-# INLINE slblsslciLoadBalancerPort #-}

-- | The Amazon Resource Number (ARN) of the SSL certificate chain to use. For
-- more information on SSL certificates, see Managing Server Certificates in
-- the AWS Identity and Access Management User Guide.
slblsslciSSLCertificateId :: Lens' SetLoadBalancerListenerSSLCertificate (Text)
slblsslciSSLCertificateId = lens _slblsslciSSLCertificateId (\s a -> s { _slblsslciSSLCertificateId = a })
{-# INLINE slblsslciSSLCertificateId #-}

instance ToQuery SetLoadBalancerListenerSSLCertificate where
    toQuery = genericQuery def

    deriving (Eq, Show, Generic)

instance AWSRequest SetLoadBalancerListenerSSLCertificate where
    type Sv SetLoadBalancerListenerSSLCertificate = ELB
    type Rs SetLoadBalancerListenerSSLCertificate = SetLoadBalancerListenerSSLCertificateResponse

    request = post "SetLoadBalancerListenerSSLCertificate"
    response _ = nullaryResponse SetLoadBalancerListenerSSLCertificateResponse
