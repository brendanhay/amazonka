{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.EC2.V2014_06_15.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Elastic Compute Cloud (Amazon EC2) is a web service that provides
-- resizable compute capacity in the cloud. It is designed to make web-scale
-- computing easier for developers. Amazon EC2’s simple web service interface
-- allows you to obtain and configure capacity with minimal friction. It
-- provides you with complete control of your computing resources and lets you
-- run on Amazon’s proven computing environment. Amazon EC2 reduces the time
-- required to obtain and boot new server instances to minutes, allowing you
-- to quickly scale capacity, both up and down, as your computing requirements
-- change. Amazon EC2 changes the economics of computing by allowing you to
-- pay only for capacity that you actually use. Amazon EC2 provides developers
-- the tools to build failure resilient applications and isolate themselves
-- from common failure scenarios.
module Network.AWS.EC2.V2014_06_15.Types where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2014-06-15@) of the
-- @Amazon Elastic Compute Cloud@ service.
data EC2 deriving (Typeable)

instance AWSService EC2 where
    type Sg EC2 = V4
    data Er EC2

        = EC2Client HttpException
        | EC2Serializer String
        | EC2Service String
    service = Service
        { _svcEndpoint = Regional
        , _svcPrefix   = "ec2"
        , _svcVersion  = "2014-06-15"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er EC2)
deriving instance Generic (Er EC2)

instance AWSError (Er EC2) where
    awsError = const "EC2Error"

instance ServiceError (Er EC2) where
    serviceError    = EC2Service
    clientError     = EC2Client
    serializerError = EC2Serializer

instance Exception (Er EC2)

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def
    { xmlNamespace = Just "http://ec2.amazonaws.com/doc/2014-06-15/"
    }

