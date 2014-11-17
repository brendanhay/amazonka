{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
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
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_SetLoadBalancerListenerSSLCertificate.html>
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

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types
import qualified GHC.Exts

data SetLoadBalancerListenerSSLCertificate = SetLoadBalancerListenerSSLCertificate
    { _slblsslcLoadBalancerName :: Text
    , _slblsslcLoadBalancerPort :: Int
    , _slblsslcSSLCertificateId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'SetLoadBalancerListenerSSLCertificate' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'slblsslcLoadBalancerName' @::@ 'Text'
--
-- * 'slblsslcLoadBalancerPort' @::@ 'Int'
--
-- * 'slblsslcSSLCertificateId' @::@ 'Text'
--
setLoadBalancerListenerSSLCertificate :: Text -- ^ 'slblsslcLoadBalancerName'
                                      -> Int -- ^ 'slblsslcLoadBalancerPort'
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
slblsslcLoadBalancerPort :: Lens' SetLoadBalancerListenerSSLCertificate Int
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

data SetLoadBalancerListenerSSLCertificateResponse = SetLoadBalancerListenerSSLCertificateResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'SetLoadBalancerListenerSSLCertificateResponse' constructor.
setLoadBalancerListenerSSLCertificateResponse :: SetLoadBalancerListenerSSLCertificateResponse
setLoadBalancerListenerSSLCertificateResponse = SetLoadBalancerListenerSSLCertificateResponse

instance ToPath SetLoadBalancerListenerSSLCertificate where
    toPath = const "/"

instance ToQuery SetLoadBalancerListenerSSLCertificate

instance ToHeaders SetLoadBalancerListenerSSLCertificate

instance AWSRequest SetLoadBalancerListenerSSLCertificate where
    type Sv SetLoadBalancerListenerSSLCertificate = ELB
    type Rs SetLoadBalancerListenerSSLCertificate = SetLoadBalancerListenerSSLCertificateResponse

    request  = post "SetLoadBalancerListenerSSLCertificate"
    response = nullResponse SetLoadBalancerListenerSSLCertificateResponse
