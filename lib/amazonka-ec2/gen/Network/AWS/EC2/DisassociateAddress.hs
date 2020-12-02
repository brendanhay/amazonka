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
-- Module      : Network.AWS.EC2.DisassociateAddress
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates an Elastic IP address from the instance or network interface it's associated with.
--
--
-- An Elastic IP address is for use in either the EC2-Classic platform or in a VPC. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP Addresses> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- This is an idempotent operation. If you perform the operation more than once, Amazon EC2 doesn't return an error.
--
module Network.AWS.EC2.DisassociateAddress
    (
    -- * Creating a Request
      disassociateAddress
    , DisassociateAddress
    -- * Request Lenses
    , dasAssociationId
    , dasPublicIP
    , dasDryRun

    -- * Destructuring the Response
    , disassociateAddressResponse
    , DisassociateAddressResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DisassociateAddress.
--
--
--
-- /See:/ 'disassociateAddress' smart constructor.
data DisassociateAddress = DisassociateAddress'
  { _dasAssociationId :: !(Maybe Text)
  , _dasPublicIP      :: !(Maybe Text)
  , _dasDryRun        :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateAddress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dasAssociationId' - [EC2-VPC] The association ID. Required for EC2-VPC.
--
-- * 'dasPublicIP' - [EC2-Classic] The Elastic IP address. Required for EC2-Classic.
--
-- * 'dasDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
disassociateAddress
    :: DisassociateAddress
disassociateAddress =
  DisassociateAddress'
    {_dasAssociationId = Nothing, _dasPublicIP = Nothing, _dasDryRun = Nothing}


-- | [EC2-VPC] The association ID. Required for EC2-VPC.
dasAssociationId :: Lens' DisassociateAddress (Maybe Text)
dasAssociationId = lens _dasAssociationId (\ s a -> s{_dasAssociationId = a})

-- | [EC2-Classic] The Elastic IP address. Required for EC2-Classic.
dasPublicIP :: Lens' DisassociateAddress (Maybe Text)
dasPublicIP = lens _dasPublicIP (\ s a -> s{_dasPublicIP = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dasDryRun :: Lens' DisassociateAddress (Maybe Bool)
dasDryRun = lens _dasDryRun (\ s a -> s{_dasDryRun = a})

instance AWSRequest DisassociateAddress where
        type Rs DisassociateAddress =
             DisassociateAddressResponse
        request = postQuery ec2
        response = receiveNull DisassociateAddressResponse'

instance Hashable DisassociateAddress where

instance NFData DisassociateAddress where

instance ToHeaders DisassociateAddress where
        toHeaders = const mempty

instance ToPath DisassociateAddress where
        toPath = const "/"

instance ToQuery DisassociateAddress where
        toQuery DisassociateAddress'{..}
          = mconcat
              ["Action" =: ("DisassociateAddress" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "AssociationId" =: _dasAssociationId,
               "PublicIp" =: _dasPublicIP, "DryRun" =: _dasDryRun]

-- | /See:/ 'disassociateAddressResponse' smart constructor.
data DisassociateAddressResponse =
  DisassociateAddressResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateAddressResponse' with the minimum fields required to make a request.
--
disassociateAddressResponse
    :: DisassociateAddressResponse
disassociateAddressResponse = DisassociateAddressResponse'


instance NFData DisassociateAddressResponse where
