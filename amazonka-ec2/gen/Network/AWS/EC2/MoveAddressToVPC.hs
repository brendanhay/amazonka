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
-- Module      : Network.AWS.EC2.MoveAddressToVPC
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-MoveAddressToVPC.html AWS API Reference> for MoveAddressToVPC.
module Network.AWS.EC2.MoveAddressToVPC
    (
    -- * Creating a Request
      moveAddressToVPC
    , MoveAddressToVPC
    -- * Request Lenses
    , matvDryRun
    , matvPublicIP

    -- * Destructuring the Response
    , moveAddressToVPCResponse
    , MoveAddressToVPCResponse
    -- * Response Lenses
    , matvrsStatus
    , matvrsAllocationId
    , matvrsResponseStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'moveAddressToVPC' smart constructor.
data MoveAddressToVPC = MoveAddressToVPC'
    { _matvDryRun   :: !(Maybe Bool)
    , _matvPublicIP :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'MoveAddressToVPC' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'matvDryRun'
--
-- * 'matvPublicIP'
moveAddressToVPC
    :: Text -- ^ 'matvPublicIP'
    -> MoveAddressToVPC
moveAddressToVPC pPublicIP_ =
    MoveAddressToVPC'
    { _matvDryRun = Nothing
    , _matvPublicIP = pPublicIP_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
matvDryRun :: Lens' MoveAddressToVPC (Maybe Bool)
matvDryRun = lens _matvDryRun (\ s a -> s{_matvDryRun = a});

-- | The Elastic IP address.
matvPublicIP :: Lens' MoveAddressToVPC Text
matvPublicIP = lens _matvPublicIP (\ s a -> s{_matvPublicIP = a});

instance AWSRequest MoveAddressToVPC where
        type Rs MoveAddressToVPC = MoveAddressToVPCResponse
        request = postQuery eC2
        response
          = receiveXML
              (\ s h x ->
                 MoveAddressToVPCResponse' <$>
                   (x .@? "status") <*> (x .@? "allocationId") <*>
                     (pure (fromEnum s)))

instance ToHeaders MoveAddressToVPC where
        toHeaders = const mempty

instance ToPath MoveAddressToVPC where
        toPath = const "/"

instance ToQuery MoveAddressToVPC where
        toQuery MoveAddressToVPC'{..}
          = mconcat
              ["Action" =: ("MoveAddressToVpc" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _matvDryRun, "PublicIp" =: _matvPublicIP]

-- | /See:/ 'moveAddressToVPCResponse' smart constructor.
data MoveAddressToVPCResponse = MoveAddressToVPCResponse'
    { _matvrsStatus         :: !(Maybe AddressStatus)
    , _matvrsAllocationId   :: !(Maybe Text)
    , _matvrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'MoveAddressToVPCResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'matvrsStatus'
--
-- * 'matvrsAllocationId'
--
-- * 'matvrsResponseStatus'
moveAddressToVPCResponse
    :: Int -- ^ 'matvrsResponseStatus'
    -> MoveAddressToVPCResponse
moveAddressToVPCResponse pResponseStatus_ =
    MoveAddressToVPCResponse'
    { _matvrsStatus = Nothing
    , _matvrsAllocationId = Nothing
    , _matvrsResponseStatus = pResponseStatus_
    }

-- | The status of the move of the IP address.
matvrsStatus :: Lens' MoveAddressToVPCResponse (Maybe AddressStatus)
matvrsStatus = lens _matvrsStatus (\ s a -> s{_matvrsStatus = a});

-- | The allocation ID for the Elastic IP address.
matvrsAllocationId :: Lens' MoveAddressToVPCResponse (Maybe Text)
matvrsAllocationId = lens _matvrsAllocationId (\ s a -> s{_matvrsAllocationId = a});

-- | The response status code.
matvrsResponseStatus :: Lens' MoveAddressToVPCResponse Int
matvrsResponseStatus = lens _matvrsResponseStatus (\ s a -> s{_matvrsResponseStatus = a});
