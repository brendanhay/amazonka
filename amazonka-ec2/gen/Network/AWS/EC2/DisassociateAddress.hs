{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.DisassociateAddress
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

-- | Disassociates an Elastic IP address from the instance or network
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
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DisassociateAddress.html>
module Network.AWS.EC2.DisassociateAddress
    (
    -- * Request
      DisassociateAddress
    -- ** Request constructor
    , disassociateAddress
    -- ** Request lenses
    , disaAssociationId
    , disaPublicIP
    , disaDryRun

    -- * Response
    , DisassociateAddressResponse
    -- ** Response constructor
    , disassociateAddressResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.EC2.Types

-- | /See:/ 'disassociateAddress' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'disaAssociationId'
--
-- * 'disaPublicIP'
--
-- * 'disaDryRun'
data DisassociateAddress = DisassociateAddress'{_disaAssociationId :: Maybe Text, _disaPublicIP :: Maybe Text, _disaDryRun :: Maybe Bool} deriving (Eq, Read, Show)

-- | 'DisassociateAddress' smart constructor.
disassociateAddress :: DisassociateAddress
disassociateAddress = DisassociateAddress'{_disaAssociationId = Nothing, _disaPublicIP = Nothing, _disaDryRun = Nothing};

-- | [EC2-VPC] The association ID. Required for EC2-VPC.
disaAssociationId :: Lens' DisassociateAddress (Maybe Text)
disaAssociationId = lens _disaAssociationId (\ s a -> s{_disaAssociationId = a});

-- | [EC2-Classic] The Elastic IP address. Required for EC2-Classic.
disaPublicIP :: Lens' DisassociateAddress (Maybe Text)
disaPublicIP = lens _disaPublicIP (\ s a -> s{_disaPublicIP = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
disaDryRun :: Lens' DisassociateAddress (Maybe Bool)
disaDryRun = lens _disaDryRun (\ s a -> s{_disaDryRun = a});

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
               "AssociationId" =: _disaAssociationId,
               "PublicIp" =: _disaPublicIP, "DryRun" =: _disaDryRun]

-- | /See:/ 'disassociateAddressResponse' smart constructor.
data DisassociateAddressResponse = DisassociateAddressResponse' deriving (Eq, Read, Show)

-- | 'DisassociateAddressResponse' smart constructor.
disassociateAddressResponse :: DisassociateAddressResponse
disassociateAddressResponse = DisassociateAddressResponse';
