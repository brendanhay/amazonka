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
    , mkCheckDNSAvailability
    -- ** Request lenses
    , cdnsaCNAMEPrefix

    -- * Response
    , CheckDNSAvailabilityResponse
    -- ** Response lenses
    , cdnsarsAvailable
    , cdnsarsFullyQualifiedCNAME
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Results message indicating whether a CNAME is available.
newtype CheckDNSAvailability = CheckDNSAvailability
    { _cdnsaCNAMEPrefix :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CheckDNSAvailability' request.
mkCheckDNSAvailability :: Text -- ^ 'cdnsaCNAMEPrefix'
                       -> CheckDNSAvailability
mkCheckDNSAvailability p1 = CheckDNSAvailability
    { _cdnsaCNAMEPrefix = p1
    }
{-# INLINE mkCheckDNSAvailability #-}

-- | The prefix used when this CNAME is reserved.
cdnsaCNAMEPrefix :: Lens' CheckDNSAvailability Text
cdnsaCNAMEPrefix =
    lens _cdnsaCNAMEPrefix (\s a -> s { _cdnsaCNAMEPrefix = a })
{-# INLINE cdnsaCNAMEPrefix #-}

instance ToQuery CheckDNSAvailability where
    toQuery = genericQuery def

-- | Indicates if the specified CNAME is available.
data CheckDNSAvailabilityResponse = CheckDNSAvailabilityResponse
    { _cdnsarsAvailable :: Maybe Bool
    , _cdnsarsFullyQualifiedCNAME :: Maybe Text
    } deriving (Show, Generic)

-- | Indicates if the specified CNAME is available: true : The CNAME is
-- available. true : The CNAME is not available. true : The CNAME is
-- available. false : The CNAME is not available.
cdnsarsAvailable :: Lens' CheckDNSAvailabilityResponse (Maybe Bool)
cdnsarsAvailable =
    lens _cdnsarsAvailable (\s a -> s { _cdnsarsAvailable = a })
{-# INLINE cdnsarsAvailable #-}

-- | The fully qualified CNAME to reserve when CreateEnvironment is called with
-- the provided prefix.
cdnsarsFullyQualifiedCNAME :: Lens' CheckDNSAvailabilityResponse (Maybe Text)
cdnsarsFullyQualifiedCNAME =
    lens _cdnsarsFullyQualifiedCNAME
         (\s a -> s { _cdnsarsFullyQualifiedCNAME = a })
{-# INLINE cdnsarsFullyQualifiedCNAME #-}

instance FromXML CheckDNSAvailabilityResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CheckDNSAvailability where
    type Sv CheckDNSAvailability = ElasticBeanstalk
    type Rs CheckDNSAvailability = CheckDNSAvailabilityResponse

    request = post "CheckDNSAvailability"
    response _ = xmlResponse
