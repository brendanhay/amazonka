{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
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
module Network.AWS.ElasticBeanstalk.V2010_12_01.CheckDNSAvailability
    (
    -- * Request
      CheckDNSAvailability
    -- ** Request constructor
    , checkDNSAvailability
    -- ** Request lenses
    , cdnsamCNAMEPrefix

    -- * Response
    , CheckDNSAvailabilityResponse
    -- ** Response lenses
    , cdnsarmAvailable
    , cdnsarmFullyQualifiedCNAME
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CheckDNSAvailability' request.
checkDNSAvailability :: Text -- ^ 'cdnsamCNAMEPrefix'
                     -> CheckDNSAvailability
checkDNSAvailability p1 = CheckDNSAvailability
    { _cdnsamCNAMEPrefix = p1
    }
{-# INLINE checkDNSAvailability #-}

data CheckDNSAvailability = CheckDNSAvailability
    { _cdnsamCNAMEPrefix :: Text
      -- ^ The prefix used when this CNAME is reserved.
    } deriving (Show, Generic)

-- | The prefix used when this CNAME is reserved.
cdnsamCNAMEPrefix :: Lens' CheckDNSAvailability (Text)
cdnsamCNAMEPrefix f x =
    f (_cdnsamCNAMEPrefix x)
        <&> \y -> x { _cdnsamCNAMEPrefix = y }
{-# INLINE cdnsamCNAMEPrefix #-}

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

-- | Indicates if the specified CNAME is available: true : The CNAME is
-- available. true : The CNAME is not available. true : The CNAME is
-- available. false : The CNAME is not available.
cdnsarmAvailable :: Lens' CheckDNSAvailabilityResponse (Maybe Bool)
cdnsarmAvailable f x =
    f (_cdnsarmAvailable x)
        <&> \y -> x { _cdnsarmAvailable = y }
{-# INLINE cdnsarmAvailable #-}

-- | The fully qualified CNAME to reserve when CreateEnvironment is called with
-- the provided prefix.
cdnsarmFullyQualifiedCNAME :: Lens' CheckDNSAvailabilityResponse (Maybe Text)
cdnsarmFullyQualifiedCNAME f x =
    f (_cdnsarmFullyQualifiedCNAME x)
        <&> \y -> x { _cdnsarmFullyQualifiedCNAME = y }
{-# INLINE cdnsarmFullyQualifiedCNAME #-}

instance FromXML CheckDNSAvailabilityResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CheckDNSAvailability where
    type Sv CheckDNSAvailability = ElasticBeanstalk
    type Rs CheckDNSAvailability = CheckDNSAvailabilityResponse

    request = post "CheckDNSAvailability"
    response _ = xmlResponse
