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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates an Elastic IP address from the instance or network
-- interface it\'s associated with.
--
-- An Elastic IP address is for use in either the EC2-Classic platform or
-- in a VPC. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP Addresses>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- This is an idempotent operation. If you perform the operation more than
-- once, Amazon EC2 doesn\'t return an error.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DisassociateAddress.html AWS API Reference> for DisassociateAddress.
module Network.AWS.EC2.DisassociateAddress
    (
    -- * Creating a Request
      DisassociateAddress
    , disassociateAddress
    -- * Request Lenses
    , dasAssociationId
    , dasPublicIP
    , dasDryRun

    -- * Destructuring the Response
    , DisassociateAddressResponse
    , disassociateAddressResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disassociateAddress' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dasAssociationId'
--
-- * 'dasPublicIP'
--
-- * 'dasDryRun'
data DisassociateAddress = DisassociateAddress'
    { _dasAssociationId :: !(Maybe Text)
    , _dasPublicIP :: !(Maybe Text)
    , _dasDryRun :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DisassociateAddress' smart constructor.
disassociateAddress :: DisassociateAddress
disassociateAddress = 
    DisassociateAddress'
    { _dasAssociationId = Nothing
    , _dasPublicIP = Nothing
    , _dasDryRun = Nothing
    }

-- | [EC2-VPC] The association ID. Required for EC2-VPC.
dasAssociationId :: Lens' DisassociateAddress (Maybe Text)
dasAssociationId = lens _dasAssociationId (\ s a -> s{_dasAssociationId = a});

-- | [EC2-Classic] The Elastic IP address. Required for EC2-Classic.
dasPublicIP :: Lens' DisassociateAddress (Maybe Text)
dasPublicIP = lens _dasPublicIP (\ s a -> s{_dasPublicIP = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dasDryRun :: Lens' DisassociateAddress (Maybe Bool)
dasDryRun = lens _dasDryRun (\ s a -> s{_dasDryRun = a});

instance AWSRequest DisassociateAddress where
        type Sv DisassociateAddress = EC2
        type Rs DisassociateAddress =
             DisassociateAddressResponse
        request = post
        response = receiveNull DisassociateAddressResponse'

instance ToHeaders DisassociateAddress where
        toHeaders = const mempty

instance ToPath DisassociateAddress where
        toPath = const "/"

instance ToQuery DisassociateAddress where
        toQuery DisassociateAddress'{..}
          = mconcat
              ["Action" =: ("DisassociateAddress" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "AssociationId" =: _dasAssociationId,
               "PublicIp" =: _dasPublicIP, "DryRun" =: _dasDryRun]

-- | /See:/ 'disassociateAddressResponse' smart constructor.
data DisassociateAddressResponse =
    DisassociateAddressResponse' 
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DisassociateAddressResponse' smart constructor.
disassociateAddressResponse :: DisassociateAddressResponse
disassociateAddressResponse = DisassociateAddressResponse'
