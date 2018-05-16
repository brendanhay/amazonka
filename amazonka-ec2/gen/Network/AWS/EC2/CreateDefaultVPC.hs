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
-- Module      : Network.AWS.EC2.CreateDefaultVPC
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a default VPC with a size @/16@ IPv4 CIDR block and a default subnet in each Availability Zone. For more information about the components of a default VPC, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/default-vpc.html Default VPC and Default Subnets> in the /Amazon Virtual Private Cloud User Guide/ . You cannot specify the components of the default VPC yourself.
--
--
-- You can create a default VPC if you deleted your previous default VPC. You cannot have more than one default VPC per region.
--
-- If your account supports EC2-Classic, you cannot use this action to create a default VPC in a region that supports EC2-Classic. If you want a default VPC in a region that supports EC2-Classic, see "I really want a default VPC for my existing EC2 account. Is that possible?" in the <http://aws.amazon.com/vpc/faqs/#Default_VPCs Default VPCs FAQ> .
--
module Network.AWS.EC2.CreateDefaultVPC
    (
    -- * Creating a Request
      createDefaultVPC
    , CreateDefaultVPC
    -- * Request Lenses
    , cdvDryRun

    -- * Destructuring the Response
    , createDefaultVPCResponse
    , CreateDefaultVPCResponse
    -- * Response Lenses
    , cdvrsVPC
    , cdvrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for CreateDefaultVpc.
--
--
--
-- /See:/ 'createDefaultVPC' smart constructor.
newtype CreateDefaultVPC = CreateDefaultVPC'
  { _cdvDryRun :: Maybe Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDefaultVPC' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdvDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
createDefaultVPC
    :: CreateDefaultVPC
createDefaultVPC = CreateDefaultVPC' {_cdvDryRun = Nothing}


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
cdvDryRun :: Lens' CreateDefaultVPC (Maybe Bool)
cdvDryRun = lens _cdvDryRun (\ s a -> s{_cdvDryRun = a})

instance AWSRequest CreateDefaultVPC where
        type Rs CreateDefaultVPC = CreateDefaultVPCResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CreateDefaultVPCResponse' <$>
                   (x .@? "vpc") <*> (pure (fromEnum s)))

instance Hashable CreateDefaultVPC where

instance NFData CreateDefaultVPC where

instance ToHeaders CreateDefaultVPC where
        toHeaders = const mempty

instance ToPath CreateDefaultVPC where
        toPath = const "/"

instance ToQuery CreateDefaultVPC where
        toQuery CreateDefaultVPC'{..}
          = mconcat
              ["Action" =: ("CreateDefaultVpc" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _cdvDryRun]

-- | Contains the output of CreateDefaultVpc.
--
--
--
-- /See:/ 'createDefaultVPCResponse' smart constructor.
data CreateDefaultVPCResponse = CreateDefaultVPCResponse'
  { _cdvrsVPC            :: !(Maybe VPC)
  , _cdvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDefaultVPCResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdvrsVPC' - Information about the VPC.
--
-- * 'cdvrsResponseStatus' - -- | The response status code.
createDefaultVPCResponse
    :: Int -- ^ 'cdvrsResponseStatus'
    -> CreateDefaultVPCResponse
createDefaultVPCResponse pResponseStatus_ =
  CreateDefaultVPCResponse'
    {_cdvrsVPC = Nothing, _cdvrsResponseStatus = pResponseStatus_}


-- | Information about the VPC.
cdvrsVPC :: Lens' CreateDefaultVPCResponse (Maybe VPC)
cdvrsVPC = lens _cdvrsVPC (\ s a -> s{_cdvrsVPC = a})

-- | -- | The response status code.
cdvrsResponseStatus :: Lens' CreateDefaultVPCResponse Int
cdvrsResponseStatus = lens _cdvrsResponseStatus (\ s a -> s{_cdvrsResponseStatus = a})

instance NFData CreateDefaultVPCResponse where
