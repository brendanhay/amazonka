{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.AssociateDHCPOptions
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

-- | Associates a set of DHCP options (that you\'ve previously created) with
-- the specified VPC, or associates no DHCP options with the VPC.
--
-- After you associate the options with the VPC, any existing instances and
-- all new instances that you launch in that VPC use the options. You
-- don\'t need to restart or relaunch the instances. They automatically
-- pick up the changes within a few hours, depending on how frequently the
-- instance renews its DHCP lease. You can explicitly renew the lease using
-- the operating system on the instance.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_DHCP_Options.html DHCP Options Sets>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AssociateDHCPOptions.html>
module Network.AWS.EC2.AssociateDHCPOptions
    (
    -- * Request
      AssociateDHCPOptions
    -- ** Request constructor
    , associateDHCPOptions
    -- ** Request lenses
    , adoDryRun
    , adoDHCPOptionsId
    , adoVPCId

    -- * Response
    , AssociateDHCPOptionsResponse
    -- ** Response constructor
    , associateDHCPOptionsResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'associateDHCPOptions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'adoDryRun'
--
-- * 'adoDHCPOptionsId'
--
-- * 'adoVPCId'
data AssociateDHCPOptions = AssociateDHCPOptions'
    { _adoDryRun        :: !(Maybe Bool)
    , _adoDHCPOptionsId :: !Text
    , _adoVPCId         :: !Text
    } deriving (Eq,Read,Show)

-- | 'AssociateDHCPOptions' smart constructor.
associateDHCPOptions :: Text -> Text -> AssociateDHCPOptions
associateDHCPOptions pDHCPOptionsId pVPCId =
    AssociateDHCPOptions'
    { _adoDryRun = Nothing
    , _adoDHCPOptionsId = pDHCPOptionsId
    , _adoVPCId = pVPCId
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
adoDryRun :: Lens' AssociateDHCPOptions (Maybe Bool)
adoDryRun = lens _adoDryRun (\ s a -> s{_adoDryRun = a});

-- | The ID of the DHCP options set, or @default@ to associate no DHCP
-- options with the VPC.
adoDHCPOptionsId :: Lens' AssociateDHCPOptions Text
adoDHCPOptionsId = lens _adoDHCPOptionsId (\ s a -> s{_adoDHCPOptionsId = a});

-- | The ID of the VPC.
adoVPCId :: Lens' AssociateDHCPOptions Text
adoVPCId = lens _adoVPCId (\ s a -> s{_adoVPCId = a});

instance AWSRequest AssociateDHCPOptions where
        type Sv AssociateDHCPOptions = EC2
        type Rs AssociateDHCPOptions =
             AssociateDHCPOptionsResponse
        request = post
        response = receiveNull AssociateDHCPOptionsResponse'

instance ToHeaders AssociateDHCPOptions where
        toHeaders = const mempty

instance ToPath AssociateDHCPOptions where
        toPath = const "/"

instance ToQuery AssociateDHCPOptions where
        toQuery AssociateDHCPOptions'{..}
          = mconcat
              ["Action" =: ("AssociateDHCPOptions" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _adoDryRun,
               "DhcpOptionsId" =: _adoDHCPOptionsId,
               "VpcId" =: _adoVPCId]

-- | /See:/ 'associateDHCPOptionsResponse' smart constructor.
data AssociateDHCPOptionsResponse =
    AssociateDHCPOptionsResponse'
    deriving (Eq,Read,Show)

-- | 'AssociateDHCPOptionsResponse' smart constructor.
associateDHCPOptionsResponse :: AssociateDHCPOptionsResponse
associateDHCPOptionsResponse = AssociateDHCPOptionsResponse'
