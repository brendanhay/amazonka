{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
module Network.AWS.ELB.SetLoadBalancerListenerSSLCertificate
    (
    -- * Request
      SetLoadBalancerListenerSSLCertificateInput
    -- ** Request constructor
    , setLoadBalancerListenerSSLCertificateInput
    -- ** Request lenses
    , slblsslciLoadBalancerName
    , slblsslciLoadBalancerPort
    , slblsslciSSLCertificateId

    -- * Response
    , SetLoadBalancerListenerSSLCertificateResponse
    -- ** Response constructor
    , setLoadBalancerListenerSSLCertificateResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types

data SetLoadBalancerListenerSSLCertificateInput = SetLoadBalancerListenerSSLCertificateInput
    { _slblsslciLoadBalancerName :: Text
    , _slblsslciLoadBalancerPort :: Int
    , _slblsslciSSLCertificateId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'SetLoadBalancerListenerSSLCertificateInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'slblsslciLoadBalancerName' @::@ 'Text'
--
-- * 'slblsslciLoadBalancerPort' @::@ 'Int'
--
-- * 'slblsslciSSLCertificateId' @::@ 'Text'
--
setLoadBalancerListenerSSLCertificateInput :: Text -- ^ 'slblsslciLoadBalancerName'
                                           -> Int -- ^ 'slblsslciLoadBalancerPort'
                                           -> Text -- ^ 'slblsslciSSLCertificateId'
                                           -> SetLoadBalancerListenerSSLCertificateInput
setLoadBalancerListenerSSLCertificateInput p1 p2 p3 = SetLoadBalancerListenerSSLCertificateInput
    { _slblsslciLoadBalancerName = p1
    , _slblsslciLoadBalancerPort = p2
    , _slblsslciSSLCertificateId = p3
    }

-- | The name of the load balancer.
slblsslciLoadBalancerName :: Lens' SetLoadBalancerListenerSSLCertificateInput Text
slblsslciLoadBalancerName =
    lens _slblsslciLoadBalancerName
        (\s a -> s { _slblsslciLoadBalancerName = a })

-- | The port that uses the specified SSL certificate.
slblsslciLoadBalancerPort :: Lens' SetLoadBalancerListenerSSLCertificateInput Int
slblsslciLoadBalancerPort =
    lens _slblsslciLoadBalancerPort
        (\s a -> s { _slblsslciLoadBalancerPort = a })

-- | The Amazon Resource Number (ARN) of the SSL certificate chain to use. For
-- more information on SSL certificates, see Managing Server Certificates in
-- the AWS Identity and Access Management User Guide.
slblsslciSSLCertificateId :: Lens' SetLoadBalancerListenerSSLCertificateInput Text
slblsslciSSLCertificateId =
    lens _slblsslciSSLCertificateId
        (\s a -> s { _slblsslciSSLCertificateId = a })

instance ToPath SetLoadBalancerListenerSSLCertificateInput where
    toPath = const "/"

instance ToQuery SetLoadBalancerListenerSSLCertificateInput

data SetLoadBalancerListenerSSLCertificateResponse = SetLoadBalancerListenerSSLCertificateResponse

-- | 'SetLoadBalancerListenerSSLCertificateResponse' constructor.
setLoadBalancerListenerSSLCertificateResponse :: SetLoadBalancerListenerSSLCertificateResponse
setLoadBalancerListenerSSLCertificateResponse = SetLoadBalancerListenerSSLCertificateResponse

instance AWSRequest SetLoadBalancerListenerSSLCertificateInput where
    type Sv SetLoadBalancerListenerSSLCertificateInput = ELB
    type Rs SetLoadBalancerListenerSSLCertificateInput = SetLoadBalancerListenerSSLCertificateResponse

    request  = post "SetLoadBalancerListenerSSLCertificate"
    response = const (nullaryResponse SetLoadBalancerListenerSSLCertificateResponse)
