{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data CheckDNSAvailability = CheckDNSAvailability
    { _cdnsamCNAMEPrefix :: Text
      -- ^ The prefix used when this CNAME is reserved.
    } deriving (Generic)

instance ToQuery CheckDNSAvailability where
    toQuery = genericToQuery def

instance AWSRequest CheckDNSAvailability where
    type Sv CheckDNSAvailability = ElasticBeanstalk
    type Rs CheckDNSAvailability = CheckDNSAvailabilityResponse

    request = post "CheckDNSAvailability"
    response _ = xmlResponse

data CheckDNSAvailabilityResponse = CheckDNSAvailabilityResponse
    { _cdnsarmAvailable :: Maybe Bool
      -- ^ Indicates if the specified CNAME is available: true : The CNAME
      -- is available. true : The CNAME is not available. true : The CNAME
      -- is available. false : The CNAME is not available.
    , _cdnsarmFullyQualifiedCNAME :: Maybe Text
      -- ^ The fully qualified CNAME to reserve when CreateEnvironment is
      -- called with the provided prefix.
    } deriving (Generic)

instance FromXML CheckDNSAvailabilityResponse where
    fromXMLOptions = xmlOptions
