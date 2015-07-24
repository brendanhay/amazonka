{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.MoveAddressToVPC
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Moves an Elastic IP address from the EC2-Classic platform to the EC2-VPC
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
    , matvrsAllocationId
    , matvrsStatus
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
moveAddressToVPC pPublicIP_ =
    MoveAddressToVPC'
    { _matvDryRun = Nothing
    , _matvPublicIP = pPublicIP_
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
        request = post "MoveAddressToVPC"
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
-- * 'matvrsAllocationId'
--
-- * 'matvrsStatus'
data MoveAddressToVPCResponse = MoveAddressToVPCResponse'
    { _matvrsAllocationId :: !(Maybe Text)
    , _matvrsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'MoveAddressToVPCResponse' smart constructor.
moveAddressToVPCResponse :: Int -> MoveAddressToVPCResponse
moveAddressToVPCResponse pStatus_ =
    MoveAddressToVPCResponse'
    { _matvrsAllocationId = Nothing
    , _matvrsStatus = pStatus_
    }

-- | The allocation ID for the Elastic IP address.
matvrsAllocationId :: Lens' MoveAddressToVPCResponse (Maybe Text)
matvrsAllocationId = lens _matvrsAllocationId (\ s a -> s{_matvrsAllocationId = a});

-- | FIXME: Undocumented member.
matvrsStatus :: Lens' MoveAddressToVPCResponse Int
matvrsStatus = lens _matvrsStatus (\ s a -> s{_matvrsStatus = a});
