{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.CheckDNSAvailability
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Checks if the specified CNAME is available.
--
-- /See:/ <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_CheckDNSAvailability.html AWS API Reference> for CheckDNSAvailability.
module Network.AWS.ElasticBeanstalk.CheckDNSAvailability
    (
    -- * Creating a Request
      CheckDNSAvailability
    , checkDNSAvailability
    -- * Request Lenses
    , cdaCNAMEPrefix

    -- * Destructuring the Response
    , CheckDNSAvailabilityResponse
    , checkDNSAvailabilityResponse
    -- * Response Lenses
    , cdarsFullyQualifiedCNAME
    , cdarsAvailable
    , cdarsStatus
    ) where

import           Network.AWS.ElasticBeanstalk.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Results message indicating whether a CNAME is available.
--
-- /See:/ 'checkDNSAvailability' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdaCNAMEPrefix'
newtype CheckDNSAvailability = CheckDNSAvailability'
    { _cdaCNAMEPrefix :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CheckDNSAvailability' smart constructor.
checkDNSAvailability :: Text -> CheckDNSAvailability
checkDNSAvailability pCNAMEPrefix_ =
    CheckDNSAvailability'
    { _cdaCNAMEPrefix = pCNAMEPrefix_
    }

-- | The prefix used when this CNAME is reserved.
cdaCNAMEPrefix :: Lens' CheckDNSAvailability Text
cdaCNAMEPrefix = lens _cdaCNAMEPrefix (\ s a -> s{_cdaCNAMEPrefix = a});

instance AWSRequest CheckDNSAvailability where
        type Sv CheckDNSAvailability = ElasticBeanstalk
        type Rs CheckDNSAvailability =
             CheckDNSAvailabilityResponse
        request = postQuery
        response
          = receiveXMLWrapper "CheckDNSAvailabilityResult"
              (\ s h x ->
                 CheckDNSAvailabilityResponse' <$>
                   (x .@? "FullyQualifiedCNAME") <*> (x .@? "Available")
                     <*> (pure (fromEnum s)))

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

-- | Indicates if the specified CNAME is available.
--
-- /See:/ 'checkDNSAvailabilityResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdarsFullyQualifiedCNAME'
--
-- * 'cdarsAvailable'
--
-- * 'cdarsStatus'
data CheckDNSAvailabilityResponse = CheckDNSAvailabilityResponse'
    { _cdarsFullyQualifiedCNAME :: !(Maybe Text)
    , _cdarsAvailable           :: !(Maybe Bool)
    , _cdarsStatus              :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CheckDNSAvailabilityResponse' smart constructor.
checkDNSAvailabilityResponse :: Int -> CheckDNSAvailabilityResponse
checkDNSAvailabilityResponse pStatus_ =
    CheckDNSAvailabilityResponse'
    { _cdarsFullyQualifiedCNAME = Nothing
    , _cdarsAvailable = Nothing
    , _cdarsStatus = pStatus_
    }

-- | The fully qualified CNAME to reserve when CreateEnvironment is called
-- with the provided prefix.
cdarsFullyQualifiedCNAME :: Lens' CheckDNSAvailabilityResponse (Maybe Text)
cdarsFullyQualifiedCNAME = lens _cdarsFullyQualifiedCNAME (\ s a -> s{_cdarsFullyQualifiedCNAME = a});

-- | Indicates if the specified CNAME is available:
--
-- @true@ : The CNAME is available.
--
-- @true@ : The CNAME is not available.
--
-- -   @true@ : The CNAME is available.
-- -   @false@ : The CNAME is not available.
cdarsAvailable :: Lens' CheckDNSAvailabilityResponse (Maybe Bool)
cdarsAvailable = lens _cdarsAvailable (\ s a -> s{_cdarsAvailable = a});

-- | Undocumented member.
cdarsStatus :: Lens' CheckDNSAvailabilityResponse Int
cdarsStatus = lens _cdarsStatus (\ s a -> s{_cdarsStatus = a});
