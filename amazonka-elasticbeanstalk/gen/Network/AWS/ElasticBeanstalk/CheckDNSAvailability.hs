{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.CheckDNSAvailability
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Checks if the specified CNAME is available.
--
--
module Network.AWS.ElasticBeanstalk.CheckDNSAvailability
    (
    -- * Creating a Request
      checkDNSAvailability
    , CheckDNSAvailability
    -- * Request Lenses
    , cdaCNAMEPrefix

    -- * Destructuring the Response
    , checkDNSAvailabilityResponse
    , CheckDNSAvailabilityResponse
    -- * Response Lenses
    , cdarsFullyQualifiedCNAME
    , cdarsAvailable
    , cdarsResponseStatus
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.ElasticBeanstalk.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Results message indicating whether a CNAME is available.
--
--
--
-- /See:/ 'checkDNSAvailability' smart constructor.
newtype CheckDNSAvailability = CheckDNSAvailability'
  { _cdaCNAMEPrefix :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CheckDNSAvailability' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdaCNAMEPrefix' - The prefix used when this CNAME is reserved.
checkDNSAvailability
    :: Text -- ^ 'cdaCNAMEPrefix'
    -> CheckDNSAvailability
checkDNSAvailability pCNAMEPrefix_ =
  CheckDNSAvailability' {_cdaCNAMEPrefix = pCNAMEPrefix_}


-- | The prefix used when this CNAME is reserved.
cdaCNAMEPrefix :: Lens' CheckDNSAvailability Text
cdaCNAMEPrefix = lens _cdaCNAMEPrefix (\ s a -> s{_cdaCNAMEPrefix = a})

instance AWSRequest CheckDNSAvailability where
        type Rs CheckDNSAvailability =
             CheckDNSAvailabilityResponse
        request = postQuery elasticBeanstalk
        response
          = receiveXMLWrapper "CheckDNSAvailabilityResult"
              (\ s h x ->
                 CheckDNSAvailabilityResponse' <$>
                   (x .@? "FullyQualifiedCNAME") <*> (x .@? "Available")
                     <*> (pure (fromEnum s)))

instance Hashable CheckDNSAvailability where

instance NFData CheckDNSAvailability where

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
--
--
-- /See:/ 'checkDNSAvailabilityResponse' smart constructor.
data CheckDNSAvailabilityResponse = CheckDNSAvailabilityResponse'
  { _cdarsFullyQualifiedCNAME :: !(Maybe Text)
  , _cdarsAvailable           :: !(Maybe Bool)
  , _cdarsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CheckDNSAvailabilityResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdarsFullyQualifiedCNAME' - The fully qualified CNAME to reserve when 'CreateEnvironment' is called with the provided prefix.
--
-- * 'cdarsAvailable' - Indicates if the specified CNAME is available:     * @true@ : The CNAME is available.     * @false@ : The CNAME is not available.
--
-- * 'cdarsResponseStatus' - -- | The response status code.
checkDNSAvailabilityResponse
    :: Int -- ^ 'cdarsResponseStatus'
    -> CheckDNSAvailabilityResponse
checkDNSAvailabilityResponse pResponseStatus_ =
  CheckDNSAvailabilityResponse'
    { _cdarsFullyQualifiedCNAME = Nothing
    , _cdarsAvailable = Nothing
    , _cdarsResponseStatus = pResponseStatus_
    }


-- | The fully qualified CNAME to reserve when 'CreateEnvironment' is called with the provided prefix.
cdarsFullyQualifiedCNAME :: Lens' CheckDNSAvailabilityResponse (Maybe Text)
cdarsFullyQualifiedCNAME = lens _cdarsFullyQualifiedCNAME (\ s a -> s{_cdarsFullyQualifiedCNAME = a})

-- | Indicates if the specified CNAME is available:     * @true@ : The CNAME is available.     * @false@ : The CNAME is not available.
cdarsAvailable :: Lens' CheckDNSAvailabilityResponse (Maybe Bool)
cdarsAvailable = lens _cdarsAvailable (\ s a -> s{_cdarsAvailable = a})

-- | -- | The response status code.
cdarsResponseStatus :: Lens' CheckDNSAvailabilityResponse Int
cdarsResponseStatus = lens _cdarsResponseStatus (\ s a -> s{_cdarsResponseStatus = a})

instance NFData CheckDNSAvailabilityResponse where
