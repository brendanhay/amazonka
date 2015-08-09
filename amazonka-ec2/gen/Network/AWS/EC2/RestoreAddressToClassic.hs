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
-- Module      : Network.AWS.EC2.RestoreAddressToClassic
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores an Elastic IP address that was previously moved to the EC2-VPC
-- platform back to the EC2-Classic platform. You cannot move an Elastic IP
-- address that was originally allocated for use in EC2-VPC. The Elastic IP
-- address must not be associated with an instance or network interface.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RestoreAddressToClassic.html AWS API Reference> for RestoreAddressToClassic.
module Network.AWS.EC2.RestoreAddressToClassic
    (
    -- * Creating a Request
      RestoreAddressToClassic
    , restoreAddressToClassic
    -- * Request Lenses
    , ratcDryRun
    , ratcPublicIP

    -- * Destructuring the Response
    , RestoreAddressToClassicResponse
    , restoreAddressToClassicResponse
    -- * Response Lenses
    , ratcrsPublicIP
    , ratcrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'restoreAddressToClassic' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ratcDryRun'
--
-- * 'ratcPublicIP'
data RestoreAddressToClassic = RestoreAddressToClassic'
    { _ratcDryRun   :: !(Maybe Bool)
    , _ratcPublicIP :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RestoreAddressToClassic' smart constructor.
restoreAddressToClassic :: Text -> RestoreAddressToClassic
restoreAddressToClassic pPublicIP_ =
    RestoreAddressToClassic'
    { _ratcDryRun = Nothing
    , _ratcPublicIP = pPublicIP_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
ratcDryRun :: Lens' RestoreAddressToClassic (Maybe Bool)
ratcDryRun = lens _ratcDryRun (\ s a -> s{_ratcDryRun = a});

-- | The Elastic IP address.
ratcPublicIP :: Lens' RestoreAddressToClassic Text
ratcPublicIP = lens _ratcPublicIP (\ s a -> s{_ratcPublicIP = a});

instance AWSRequest RestoreAddressToClassic where
        type Sv RestoreAddressToClassic = EC2
        type Rs RestoreAddressToClassic =
             RestoreAddressToClassicResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 RestoreAddressToClassicResponse' <$>
                   (x .@? "publicIp") <*> (pure (fromEnum s)))

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
-- * 'ratcrsPublicIP'
--
-- * 'ratcrsStatus'
data RestoreAddressToClassicResponse = RestoreAddressToClassicResponse'
    { _ratcrsPublicIP :: !(Maybe Text)
    , _ratcrsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RestoreAddressToClassicResponse' smart constructor.
restoreAddressToClassicResponse :: Int -> RestoreAddressToClassicResponse
restoreAddressToClassicResponse pStatus_ =
    RestoreAddressToClassicResponse'
    { _ratcrsPublicIP = Nothing
    , _ratcrsStatus = pStatus_
    }

-- | The Elastic IP address.
ratcrsPublicIP :: Lens' RestoreAddressToClassicResponse (Maybe Text)
ratcrsPublicIP = lens _ratcrsPublicIP (\ s a -> s{_ratcrsPublicIP = a});

-- | Undocumented member.
ratcrsStatus :: Lens' RestoreAddressToClassicResponse Int
ratcrsStatus = lens _ratcrsStatus (\ s a -> s{_ratcrsStatus = a});
