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
module Network.AWS.ElasticBeanstalk.CheckDNSAvailability
    (
    -- * Request
      CheckDNSAvailabilityMessage
    -- ** Request constructor
    , checkDNSAvailabilityMessage
    -- ** Request lenses
    , cdnsamCNAMEPrefix

    -- * Response
    , CheckDNSAvailabilityResultMessage
    -- ** Response constructor
    , checkDNSAvailabilityResultMessage
    -- ** Response lenses
    , cdnsarmAvailable
    , cdnsarmFullyQualifiedCNAME
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types

newtype CheckDNSAvailabilityMessage = CheckDNSAvailabilityMessage
    { _cdnsamCNAMEPrefix :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'CheckDNSAvailabilityMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdnsamCNAMEPrefix' @::@ 'Text'
--
checkDNSAvailabilityMessage :: Text -- ^ 'cdnsamCNAMEPrefix'
                            -> CheckDNSAvailabilityMessage
checkDNSAvailabilityMessage p1 = CheckDNSAvailabilityMessage
    { _cdnsamCNAMEPrefix = p1
    }

-- | The prefix used when this CNAME is reserved.
cdnsamCNAMEPrefix :: Lens' CheckDNSAvailabilityMessage Text
cdnsamCNAMEPrefix =
    lens _cdnsamCNAMEPrefix (\s a -> s { _cdnsamCNAMEPrefix = a })
instance ToQuery CheckDNSAvailabilityMessage

instance ToPath CheckDNSAvailabilityMessage where
    toPath = const "/"

data CheckDNSAvailabilityResultMessage = CheckDNSAvailabilityResultMessage
    { _cdnsarmAvailable           :: Maybe Bool
    , _cdnsarmFullyQualifiedCNAME :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CheckDNSAvailabilityResultMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdnsarmAvailable' @::@ 'Maybe' 'Bool'
--
-- * 'cdnsarmFullyQualifiedCNAME' @::@ 'Maybe' 'Text'
--
checkDNSAvailabilityResultMessage :: CheckDNSAvailabilityResultMessage
checkDNSAvailabilityResultMessage = CheckDNSAvailabilityResultMessage
    { _cdnsarmAvailable           = Nothing
    , _cdnsarmFullyQualifiedCNAME = Nothing
    }

-- | Indicates if the specified CNAME is available: true : The CNAME is
-- available. true : The CNAME is not available. true : The CNAME is
-- available. false : The CNAME is not available.
cdnsarmAvailable :: Lens' CheckDNSAvailabilityResultMessage (Maybe Bool)
cdnsarmAvailable = lens _cdnsarmAvailable (\s a -> s { _cdnsarmAvailable = a })

-- | The fully qualified CNAME to reserve when CreateEnvironment is called
-- with the provided prefix.
cdnsarmFullyQualifiedCNAME :: Lens' CheckDNSAvailabilityResultMessage (Maybe Text)
cdnsarmFullyQualifiedCNAME =
    lens _cdnsarmFullyQualifiedCNAME
        (\s a -> s { _cdnsarmFullyQualifiedCNAME = a })
instance FromXML CheckDNSAvailabilityResultMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CheckDNSAvailabilityResultMessage"

instance AWSRequest CheckDNSAvailabilityMessage where
    type Sv CheckDNSAvailabilityMessage = ElasticBeanstalk
    type Rs CheckDNSAvailabilityMessage = CheckDNSAvailabilityResultMessage

    request  = post "CheckDNSAvailability"
    response = xmlResponse $ \h x -> CheckDNSAvailabilityResultMessage
        <$> x %| "Available"
        <*> x %| "FullyQualifiedCNAME"
