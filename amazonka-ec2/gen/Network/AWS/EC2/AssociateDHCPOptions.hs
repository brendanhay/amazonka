{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AssociateDHCPOptions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Associates a set of DHCP options (that you\'ve previously created) with
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
    , adorqDryRun
    , adorqDHCPOptionsId
    , adorqVPCId

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
-- * 'adorqDryRun'
--
-- * 'adorqDHCPOptionsId'
--
-- * 'adorqVPCId'
data AssociateDHCPOptions = AssociateDHCPOptions'
    { _adorqDryRun        :: !(Maybe Bool)
    , _adorqDHCPOptionsId :: !Text
    , _adorqVPCId         :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AssociateDHCPOptions' smart constructor.
associateDHCPOptions :: Text -> Text -> AssociateDHCPOptions
associateDHCPOptions pDHCPOptionsId_ pVPCId_ =
    AssociateDHCPOptions'
    { _adorqDryRun = Nothing
    , _adorqDHCPOptionsId = pDHCPOptionsId_
    , _adorqVPCId = pVPCId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
adorqDryRun :: Lens' AssociateDHCPOptions (Maybe Bool)
adorqDryRun = lens _adorqDryRun (\ s a -> s{_adorqDryRun = a});

-- | The ID of the DHCP options set, or @default@ to associate no DHCP
-- options with the VPC.
adorqDHCPOptionsId :: Lens' AssociateDHCPOptions Text
adorqDHCPOptionsId = lens _adorqDHCPOptionsId (\ s a -> s{_adorqDHCPOptionsId = a});

-- | The ID of the VPC.
adorqVPCId :: Lens' AssociateDHCPOptions Text
adorqVPCId = lens _adorqVPCId (\ s a -> s{_adorqVPCId = a});

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
               "DryRun" =: _adorqDryRun,
               "DhcpOptionsId" =: _adorqDHCPOptionsId,
               "VpcId" =: _adorqVPCId]

-- | /See:/ 'associateDHCPOptionsResponse' smart constructor.
data AssociateDHCPOptionsResponse =
    AssociateDHCPOptionsResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AssociateDHCPOptionsResponse' smart constructor.
associateDHCPOptionsResponse :: AssociateDHCPOptionsResponse
associateDHCPOptionsResponse = AssociateDHCPOptionsResponse'
