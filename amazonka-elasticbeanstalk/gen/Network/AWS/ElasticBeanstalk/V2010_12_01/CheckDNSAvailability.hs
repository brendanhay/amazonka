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
    -- ** Response constructor
    , mkCheckDNSAvailabilityResponse
    -- ** Response lenses
    , cdnsarAvailable
    , cdnsarFullyQualifiedCNAME
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
--
-- The fields accessible through corresponding lenses are:
--
-- * @CNAMEPrefix ::@ @Text@
--
mkCheckDNSAvailability :: Text -- ^ 'cdnsaCNAMEPrefix'
                       -> CheckDNSAvailability
mkCheckDNSAvailability p1 = CheckDNSAvailability
    { _cdnsaCNAMEPrefix = p1
    }

-- | The prefix used when this CNAME is reserved.
cdnsaCNAMEPrefix :: Lens' CheckDNSAvailability Text
cdnsaCNAMEPrefix =
    lens _cdnsaCNAMEPrefix (\s a -> s { _cdnsaCNAMEPrefix = a })

instance ToQuery CheckDNSAvailability where
    toQuery = genericQuery def

-- | Indicates if the specified CNAME is available.
data CheckDNSAvailabilityResponse = CheckDNSAvailabilityResponse
    { _cdnsarAvailable :: Maybe Bool
    , _cdnsarFullyQualifiedCNAME :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CheckDNSAvailabilityResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Available ::@ @Maybe Bool@
--
-- * @FullyQualifiedCNAME ::@ @Maybe Text@
--
mkCheckDNSAvailabilityResponse :: CheckDNSAvailabilityResponse
mkCheckDNSAvailabilityResponse = CheckDNSAvailabilityResponse
    { _cdnsarAvailable = Nothing
    , _cdnsarFullyQualifiedCNAME = Nothing
    }

-- | Indicates if the specified CNAME is available: true : The CNAME is
-- available. true : The CNAME is not available. true : The CNAME is
-- available. false : The CNAME is not available.
cdnsarAvailable :: Lens' CheckDNSAvailabilityResponse (Maybe Bool)
cdnsarAvailable = lens _cdnsarAvailable (\s a -> s { _cdnsarAvailable = a })

-- | The fully qualified CNAME to reserve when CreateEnvironment is called with
-- the provided prefix.
cdnsarFullyQualifiedCNAME :: Lens' CheckDNSAvailabilityResponse (Maybe Text)
cdnsarFullyQualifiedCNAME =
    lens _cdnsarFullyQualifiedCNAME
         (\s a -> s { _cdnsarFullyQualifiedCNAME = a })

instance FromXML CheckDNSAvailabilityResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CheckDNSAvailability where
    type Sv CheckDNSAvailability = ElasticBeanstalk
    type Rs CheckDNSAvailability = CheckDNSAvailabilityResponse

    request = post "CheckDNSAvailability"
    response _ = xmlResponse
