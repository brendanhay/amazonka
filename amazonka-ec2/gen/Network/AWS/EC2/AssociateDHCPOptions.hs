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
-- Module      : Network.AWS.EC2.AssociateDHCPOptions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a set of DHCP options (that you've previously created) with the specified VPC, or associates no DHCP options with the VPC.
--
--
-- After you associate the options with the VPC, any existing instances and all new instances that you launch in that VPC use the options. You don't need to restart or relaunch the instances. They automatically pick up the changes within a few hours, depending on how frequently the instance renews its DHCP lease. You can explicitly renew the lease using the operating system on the instance.
--
-- For more information, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_DHCP_Options.html DHCP Options Sets> in the /Amazon Virtual Private Cloud User Guide/ .
--
module Network.AWS.EC2.AssociateDHCPOptions
    (
    -- * Creating a Request
      associateDHCPOptions
    , AssociateDHCPOptions
    -- * Request Lenses
    , adoDryRun
    , adoDHCPOptionsId
    , adoVPCId

    -- * Destructuring the Response
    , associateDHCPOptionsResponse
    , AssociateDHCPOptionsResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for AssociateDhcpOptions.
--
--
--
-- /See:/ 'associateDHCPOptions' smart constructor.
data AssociateDHCPOptions = AssociateDHCPOptions'
  { _adoDryRun        :: !(Maybe Bool)
  , _adoDHCPOptionsId :: !Text
  , _adoVPCId         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateDHCPOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adoDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'adoDHCPOptionsId' - The ID of the DHCP options set, or @default@ to associate no DHCP options with the VPC.
--
-- * 'adoVPCId' - The ID of the VPC.
associateDHCPOptions
    :: Text -- ^ 'adoDHCPOptionsId'
    -> Text -- ^ 'adoVPCId'
    -> AssociateDHCPOptions
associateDHCPOptions pDHCPOptionsId_ pVPCId_ =
  AssociateDHCPOptions'
    { _adoDryRun = Nothing
    , _adoDHCPOptionsId = pDHCPOptionsId_
    , _adoVPCId = pVPCId_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
adoDryRun :: Lens' AssociateDHCPOptions (Maybe Bool)
adoDryRun = lens _adoDryRun (\ s a -> s{_adoDryRun = a})

-- | The ID of the DHCP options set, or @default@ to associate no DHCP options with the VPC.
adoDHCPOptionsId :: Lens' AssociateDHCPOptions Text
adoDHCPOptionsId = lens _adoDHCPOptionsId (\ s a -> s{_adoDHCPOptionsId = a})

-- | The ID of the VPC.
adoVPCId :: Lens' AssociateDHCPOptions Text
adoVPCId = lens _adoVPCId (\ s a -> s{_adoVPCId = a})

instance AWSRequest AssociateDHCPOptions where
        type Rs AssociateDHCPOptions =
             AssociateDHCPOptionsResponse
        request = postQuery ec2
        response = receiveNull AssociateDHCPOptionsResponse'

instance Hashable AssociateDHCPOptions where

instance NFData AssociateDHCPOptions where

instance ToHeaders AssociateDHCPOptions where
        toHeaders = const mempty

instance ToPath AssociateDHCPOptions where
        toPath = const "/"

instance ToQuery AssociateDHCPOptions where
        toQuery AssociateDHCPOptions'{..}
          = mconcat
              ["Action" =: ("AssociateDhcpOptions" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _adoDryRun,
               "DhcpOptionsId" =: _adoDHCPOptionsId,
               "VpcId" =: _adoVPCId]

-- | /See:/ 'associateDHCPOptionsResponse' smart constructor.
data AssociateDHCPOptionsResponse =
  AssociateDHCPOptionsResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateDHCPOptionsResponse' with the minimum fields required to make a request.
--
associateDHCPOptionsResponse
    :: AssociateDHCPOptionsResponse
associateDHCPOptionsResponse = AssociateDHCPOptionsResponse'


instance NFData AssociateDHCPOptionsResponse where
