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
    , disassociateAssociationId
    , disassociatePublicIP
    , disassociateDryRun

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
-- * 'disassociateAssociationId'
--
-- * 'disassociatePublicIP'
--
-- * 'disassociateDryRun'
data DisassociateAddress = DisassociateAddress'{_disassociateAssociationId :: Maybe Text, _disassociatePublicIP :: Maybe Text, _disassociateDryRun :: Maybe Bool} deriving (Eq, Read, Show)

-- | 'DisassociateAddress' smart constructor.
disassociateAddress :: DisassociateAddress
disassociateAddress = DisassociateAddress'{_disassociateAssociationId = Nothing, _disassociatePublicIP = Nothing, _disassociateDryRun = Nothing};

-- | [EC2-VPC] The association ID. Required for EC2-VPC.
disassociateAssociationId :: Lens' DisassociateAddress (Maybe Text)
disassociateAssociationId = lens _disassociateAssociationId (\ s a -> s{_disassociateAssociationId = a});

-- | [EC2-Classic] The Elastic IP address. Required for EC2-Classic.
disassociatePublicIP :: Lens' DisassociateAddress (Maybe Text)
disassociatePublicIP = lens _disassociatePublicIP (\ s a -> s{_disassociatePublicIP = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
disassociateDryRun :: Lens' DisassociateAddress (Maybe Bool)
disassociateDryRun = lens _disassociateDryRun (\ s a -> s{_disassociateDryRun = a});

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
               "AssociationId" =: _disassociateAssociationId,
               "PublicIp" =: _disassociatePublicIP,
               "DryRun" =: _disassociateDryRun]

-- | /See:/ 'disassociateAddressResponse' smart constructor.
data DisassociateAddressResponse = DisassociateAddressResponse' deriving (Eq, Read, Show)

-- | 'DisassociateAddressResponse' smart constructor.
disassociateAddressResponse :: DisassociateAddressResponse
disassociateAddressResponse = DisassociateAddressResponse';
