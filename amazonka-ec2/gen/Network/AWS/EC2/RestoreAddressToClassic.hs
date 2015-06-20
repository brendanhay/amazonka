{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.RestoreAddressToClassic
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

-- | Restores an Elastic IP address that was previously moved to the EC2-VPC
-- platform back to the EC2-Classic platform. You cannot move an Elastic IP
-- address that was originally allocated for use in EC2-VPC. The Elastic IP
-- address must not be associated with an instance or network interface.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RestoreAddressToClassic.html>
module Network.AWS.EC2.RestoreAddressToClassic
    (
    -- * Request
      RestoreAddressToClassic
    -- ** Request constructor
    , restoreAddressToClassic
    -- ** Request lenses
    , ratcDryRun
    , ratcPublicIP

    -- * Response
    , RestoreAddressToClassicResponse
    -- ** Response constructor
    , restoreAddressToClassicResponse
    -- ** Response lenses
    , ratcrStatus
    , ratcrPublicIP
    ) where

import Network.AWS.EC2.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'restoreAddressToClassic' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ratcDryRun'
--
-- * 'ratcPublicIP'
data RestoreAddressToClassic = RestoreAddressToClassic'{_ratcDryRun :: Maybe Bool, _ratcPublicIP :: Text} deriving (Eq, Read, Show)

-- | 'RestoreAddressToClassic' smart constructor.
restoreAddressToClassic :: Text -> RestoreAddressToClassic
restoreAddressToClassic pPublicIP = RestoreAddressToClassic'{_ratcDryRun = Nothing, _ratcPublicIP = pPublicIP};

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
ratcDryRun :: Lens' RestoreAddressToClassic (Maybe Bool)
ratcDryRun = lens _ratcDryRun (\ s a -> s{_ratcDryRun = a});

-- | The Elastic IP address.
ratcPublicIP :: Lens' RestoreAddressToClassic Text
ratcPublicIP = lens _ratcPublicIP (\ s a -> s{_ratcPublicIP = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest RestoreAddressToClassic where
        type Sv RestoreAddressToClassic = EC2
        type Rs RestoreAddressToClassic =
             RestoreAddressToClassicResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 RestoreAddressToClassicResponse' <$>
                   (x .@? "status") <*> (x .@? "publicIp"))

instance ToHeaders RestoreAddressToClassic where
        toHeaders = const mempty

instance ToPath RestoreAddressToClassic where
        toPath = const "/"

instance ToQuery RestoreAddressToClassic where
        toQuery RestoreAddressToClassic'{..}
          = mconcat
              ["Action" =:
                 ("RestoreAddressToClassic" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _ratcDryRun, "PublicIp" =: _ratcPublicIP]

-- | /See:/ 'restoreAddressToClassicResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ratcrStatus'
--
-- * 'ratcrPublicIP'
data RestoreAddressToClassicResponse = RestoreAddressToClassicResponse'{_ratcrStatus :: Maybe AddressStatus, _ratcrPublicIP :: Maybe Text} deriving (Eq, Read, Show)

-- | 'RestoreAddressToClassicResponse' smart constructor.
restoreAddressToClassicResponse :: RestoreAddressToClassicResponse
restoreAddressToClassicResponse = RestoreAddressToClassicResponse'{_ratcrStatus = Nothing, _ratcrPublicIP = Nothing};

-- | The move status for the IP address.
ratcrStatus :: Lens' RestoreAddressToClassicResponse (Maybe AddressStatus)
ratcrStatus = lens _ratcrStatus (\ s a -> s{_ratcrStatus = a});

-- | The Elastic IP address.
ratcrPublicIP :: Lens' RestoreAddressToClassicResponse (Maybe Text)
ratcrPublicIP = lens _ratcrPublicIP (\ s a -> s{_ratcrPublicIP = a});
