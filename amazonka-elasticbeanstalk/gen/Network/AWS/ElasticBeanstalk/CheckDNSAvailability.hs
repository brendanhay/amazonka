{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.CheckDNSAvailability
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Checks if the specified CNAME is available.
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_CheckDNSAvailability.html>
module Network.AWS.ElasticBeanstalk.CheckDNSAvailability
    (
    -- * Request
      CheckDNSAvailability
    -- ** Request constructor
    , checkDNSAvailability
    -- ** Request lenses
    , cdnsaCNAMEPrefix

    -- * Response
    , CheckDNSAvailabilityResponse
    -- ** Response constructor
    , checkDNSAvailabilityResponse
    -- ** Response lenses
    , cdnsarAvailable
    , cdnsarFullyQualifiedCNAME
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import qualified GHC.Exts

newtype CheckDNSAvailability = CheckDNSAvailability
    { _cdnsaCNAMEPrefix :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'CheckDNSAvailability' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdnsaCNAMEPrefix' @::@ 'Text'
--
checkDNSAvailability :: Text -- ^ 'cdnsaCNAMEPrefix'
                     -> CheckDNSAvailability
checkDNSAvailability p1 = CheckDNSAvailability
    { _cdnsaCNAMEPrefix = p1
    }

-- | The prefix used when this CNAME is reserved.
cdnsaCNAMEPrefix :: Lens' CheckDNSAvailability Text
cdnsaCNAMEPrefix = lens _cdnsaCNAMEPrefix (\s a -> s { _cdnsaCNAMEPrefix = a })

data CheckDNSAvailabilityResponse = CheckDNSAvailabilityResponse
    { _cdnsarAvailable           :: Maybe Bool
    , _cdnsarFullyQualifiedCNAME :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CheckDNSAvailabilityResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdnsarAvailable' @::@ 'Maybe' 'Bool'
--
-- * 'cdnsarFullyQualifiedCNAME' @::@ 'Maybe' 'Text'
--
checkDNSAvailabilityResponse :: CheckDNSAvailabilityResponse
checkDNSAvailabilityResponse = CheckDNSAvailabilityResponse
    { _cdnsarAvailable           = Nothing
    , _cdnsarFullyQualifiedCNAME = Nothing
    }

-- | Indicates if the specified CNAME is available: true : The CNAME is
-- available. true : The CNAME is not available. true : The CNAME is
-- available. false : The CNAME is not available.
cdnsarAvailable :: Lens' CheckDNSAvailabilityResponse (Maybe Bool)
cdnsarAvailable = lens _cdnsarAvailable (\s a -> s { _cdnsarAvailable = a })

-- | The fully qualified CNAME to reserve when CreateEnvironment is called
-- with the provided prefix.
cdnsarFullyQualifiedCNAME :: Lens' CheckDNSAvailabilityResponse (Maybe Text)
cdnsarFullyQualifiedCNAME =
    lens _cdnsarFullyQualifiedCNAME
        (\s a -> s { _cdnsarFullyQualifiedCNAME = a })

instance ToPath CheckDNSAvailability where
    toPath = const "/"

instance ToQuery CheckDNSAvailability

instance ToHeaders CheckDNSAvailability

instance AWSRequest CheckDNSAvailability where
    type Sv CheckDNSAvailability = ElasticBeanstalk
    type Rs CheckDNSAvailability = CheckDNSAvailabilityResponse

    request  = post "CheckDNSAvailability"
    response = xmlResponse

instance FromXML CheckDNSAvailabilityResponse where
    parseXML = withElement "CheckDNSAvailabilityResult" $ \x ->
            <$> x .@? "Available"
            <*> x .@? "FullyQualifiedCNAME"
