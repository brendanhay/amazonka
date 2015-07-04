{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.EC2.MoveAddressToVPC
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Moves an Elastic IP address from the EC2-Classic platform to the EC2-VPC
-- platform. The Elastic IP address must be allocated to your account, and
-- it must not be associated with an instance. After the Elastic IP address
-- is moved, it is no longer available for use in the EC2-Classic platform,
-- unless you move it back using the RestoreAddressToClassic request. You
-- cannot move an Elastic IP address that\'s allocated for use in the
-- EC2-VPC platform to the EC2-Classic platform.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-MoveAddressToVPC.html>
module Network.AWS.EC2.MoveAddressToVPC
    (
    -- * Request
      MoveAddressToVPC
    -- ** Request constructor
    , moveAddressToVPC
    -- ** Request lenses
    , matvDryRun
    , matvPublicIP

    -- * Response
    , MoveAddressToVPCResponse
    -- ** Response constructor
    , moveAddressToVPCResponse
    -- ** Response lenses
    , matvrAllocationId
    , matvrStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'moveAddressToVPC' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'matvDryRun'
--
-- * 'matvPublicIP'
data MoveAddressToVPC = MoveAddressToVPC'
    { _matvDryRun   :: !(Maybe Bool)
    , _matvPublicIP :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'MoveAddressToVPC' smart constructor.
moveAddressToVPC :: Text -> MoveAddressToVPC
moveAddressToVPC pPublicIP =
    MoveAddressToVPC'
    { _matvDryRun = Nothing
    , _matvPublicIP = pPublicIP
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
matvDryRun :: Lens' MoveAddressToVPC (Maybe Bool)
matvDryRun = lens _matvDryRun (\ s a -> s{_matvDryRun = a});

-- | The Elastic IP address.
matvPublicIP :: Lens' MoveAddressToVPC Text
matvPublicIP = lens _matvPublicIP (\ s a -> s{_matvPublicIP = a});

instance AWSRequest MoveAddressToVPC where
        type Sv MoveAddressToVPC = EC2
        type Rs MoveAddressToVPC = MoveAddressToVPCResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 MoveAddressToVPCResponse' <$>
                   (x .@? "allocationId") <*> (pure (fromEnum s)))

instance ToHeaders MoveAddressToVPC where
        toHeaders = const mempty

instance ToPath MoveAddressToVPC where
        toPath = const "/"

instance ToQuery MoveAddressToVPC where
        toQuery MoveAddressToVPC'{..}
          = mconcat
              ["Action" =: ("MoveAddressToVPC" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _matvDryRun, "PublicIp" =: _matvPublicIP]

-- | /See:/ 'moveAddressToVPCResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'matvrAllocationId'
--
-- * 'matvrStatus'
data MoveAddressToVPCResponse = MoveAddressToVPCResponse'
    { _matvrAllocationId :: !(Maybe Text)
    , _matvrStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'MoveAddressToVPCResponse' smart constructor.
moveAddressToVPCResponse :: Int -> MoveAddressToVPCResponse
moveAddressToVPCResponse pStatus =
    MoveAddressToVPCResponse'
    { _matvrAllocationId = Nothing
    , _matvrStatus = pStatus
    }

-- | The allocation ID for the Elastic IP address.
matvrAllocationId :: Lens' MoveAddressToVPCResponse (Maybe Text)
matvrAllocationId = lens _matvrAllocationId (\ s a -> s{_matvrAllocationId = a});

-- | FIXME: Undocumented member.
matvrStatus :: Lens' MoveAddressToVPCResponse Int
matvrStatus = lens _matvrStatus (\ s a -> s{_matvrStatus = a});
