{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElasticBeanstalk.CheckDNSAvailability
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

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
    , cdaCNAMEPrefix

    -- * Response
    , CheckDNSAvailabilityResponse
    -- ** Response constructor
    , checkDNSAvailabilityResponse
    -- ** Response lenses
    , cdarFullyQualifiedCNAME
    , cdarAvailable
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ElasticBeanstalk.Types

-- | /See:/ 'checkDNSAvailability' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdaCNAMEPrefix'
newtype CheckDNSAvailability = CheckDNSAvailability'{_cdaCNAMEPrefix :: Text} deriving (Eq, Read, Show)

-- | 'CheckDNSAvailability' smart constructor.
checkDNSAvailability :: Text -> CheckDNSAvailability
checkDNSAvailability pCNAMEPrefix = CheckDNSAvailability'{_cdaCNAMEPrefix = pCNAMEPrefix};

-- | The prefix used when this CNAME is reserved.
cdaCNAMEPrefix :: Lens' CheckDNSAvailability Text
cdaCNAMEPrefix = lens _cdaCNAMEPrefix (\ s a -> s{_cdaCNAMEPrefix = a});

instance AWSRequest CheckDNSAvailability where
        type Sv CheckDNSAvailability = ElasticBeanstalk
        type Rs CheckDNSAvailability =
             CheckDNSAvailabilityResponse
        request = post
        response
          = receiveXMLWrapper "CheckDNSAvailabilityResult"
              (\ s h x ->
                 CheckDNSAvailabilityResponse' <$>
                   (x .@? "FullyQualifiedCNAME") <*>
                     (x .@? "Available"))

instance ToHeaders CheckDNSAvailability where
        toHeaders = const mempty

instance ToPath CheckDNSAvailability where
        toPath = const "/"

instance ToQuery CheckDNSAvailability where
        toQuery CheckDNSAvailability'{..}
          = mconcat
              ["Action" =: ("CheckDNSAvailability" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "CNAMEPrefix" =: _cdaCNAMEPrefix]

-- | /See:/ 'checkDNSAvailabilityResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdarFullyQualifiedCNAME'
--
-- * 'cdarAvailable'
data CheckDNSAvailabilityResponse = CheckDNSAvailabilityResponse'{_cdarFullyQualifiedCNAME :: Maybe Text, _cdarAvailable :: Maybe Bool} deriving (Eq, Read, Show)

-- | 'CheckDNSAvailabilityResponse' smart constructor.
checkDNSAvailabilityResponse :: CheckDNSAvailabilityResponse
checkDNSAvailabilityResponse = CheckDNSAvailabilityResponse'{_cdarFullyQualifiedCNAME = Nothing, _cdarAvailable = Nothing};

-- | The fully qualified CNAME to reserve when CreateEnvironment is called
-- with the provided prefix.
cdarFullyQualifiedCNAME :: Lens' CheckDNSAvailabilityResponse (Maybe Text)
cdarFullyQualifiedCNAME = lens _cdarFullyQualifiedCNAME (\ s a -> s{_cdarFullyQualifiedCNAME = a});

-- | Indicates if the specified CNAME is available:
--
-- @true@ : The CNAME is available.
--
-- @true@ : The CNAME is not available.
--
-- -   @true@ : The CNAME is available.
-- -   @false@ : The CNAME is not available.
cdarAvailable :: Lens' CheckDNSAvailabilityResponse (Maybe Bool)
cdarAvailable = lens _cdarAvailable (\ s a -> s{_cdarAvailable = a});
