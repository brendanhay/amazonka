{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DisassociateAddress
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DisassociateAddress.html>
module Network.AWS.EC2.DisassociateAddress
    (
    -- * Request
      DisassociateAddress
    -- ** Request constructor
    , disassociateAddress
    -- ** Request lenses
    , dasrqAssociationId
    , dasrqPublicIP
    , dasrqDryRun

    -- * Response
    , DisassociateAddressResponse
    -- ** Response constructor
    , disassociateAddressResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'disassociateAddress' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dasrqAssociationId'
--
-- * 'dasrqPublicIP'
--
-- * 'dasrqDryRun'
data DisassociateAddress = DisassociateAddress'
    { _dasrqAssociationId :: !(Maybe Text)
    , _dasrqPublicIP      :: !(Maybe Text)
    , _dasrqDryRun        :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DisassociateAddress' smart constructor.
disassociateAddress :: DisassociateAddress
disassociateAddress =
    DisassociateAddress'
    { _dasrqAssociationId = Nothing
    , _dasrqPublicIP = Nothing
    , _dasrqDryRun = Nothing
    }

-- | [EC2-VPC] The association ID. Required for EC2-VPC.
dasrqAssociationId :: Lens' DisassociateAddress (Maybe Text)
dasrqAssociationId = lens _dasrqAssociationId (\ s a -> s{_dasrqAssociationId = a});

-- | [EC2-Classic] The Elastic IP address. Required for EC2-Classic.
dasrqPublicIP :: Lens' DisassociateAddress (Maybe Text)
dasrqPublicIP = lens _dasrqPublicIP (\ s a -> s{_dasrqPublicIP = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dasrqDryRun :: Lens' DisassociateAddress (Maybe Bool)
dasrqDryRun = lens _dasrqDryRun (\ s a -> s{_dasrqDryRun = a});

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
               "AssociationId" =: _dasrqAssociationId,
               "PublicIp" =: _dasrqPublicIP,
               "DryRun" =: _dasrqDryRun]

-- | /See:/ 'disassociateAddressResponse' smart constructor.
data DisassociateAddressResponse =
    DisassociateAddressResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DisassociateAddressResponse' smart constructor.
disassociateAddressResponse :: DisassociateAddressResponse
disassociateAddressResponse = DisassociateAddressResponse'
