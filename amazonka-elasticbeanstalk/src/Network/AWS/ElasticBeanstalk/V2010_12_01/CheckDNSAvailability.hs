{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- Module      : Network.AWS.ElasticBeanstalk.V2010_12_01.CheckDNSAvailability
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Checks if the specified CNAME is available.
-- https://elasticbeanstalk.us-east-1.amazon.com/?CNAMEPrefix=sampleapplication
-- &Operation=CheckDNSAvailability &AuthParams
-- sampleapplication.elasticbeanstalk.amazonaws.com true
-- 12f6701f-f1d6-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.V2010_12_01.CheckDNSAvailability where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

data CheckDNSAvailability = CheckDNSAvailability
    { _cdnsamCNAMEPrefix :: Text
      -- ^ The prefix used when this CNAME is reserved.
    } deriving (Show, Generic)

makeLenses ''CheckDNSAvailability

instance ToQuery CheckDNSAvailability where
    toQuery = genericQuery def

data CheckDNSAvailabilityResponse = CheckDNSAvailabilityResponse
    { _cdnsarmAvailable :: Maybe Bool
      -- ^ Indicates if the specified CNAME is available: true : The CNAME
      -- is available. true : The CNAME is not available. true : The CNAME
      -- is available. false : The CNAME is not available.
    , _cdnsarmFullyQualifiedCNAME :: Maybe Text
      -- ^ The fully qualified CNAME to reserve when CreateEnvironment is
      -- called with the provided prefix.
    } deriving (Show, Generic)

makeLenses ''CheckDNSAvailabilityResponse

instance FromXML CheckDNSAvailabilityResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CheckDNSAvailability where
    type Sv CheckDNSAvailability = ElasticBeanstalk
    type Rs CheckDNSAvailability = CheckDNSAvailabilityResponse

    request = post "CheckDNSAvailability"
    response _ = xmlResponse
