{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.ModifyVPCAttribute
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

-- | Modifies the specified attribute of the specified VPC.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifyVPCAttribute.html>
module Network.AWS.EC2.ModifyVPCAttribute
    (
    -- * Request
      ModifyVPCAttribute
    -- ** Request constructor
    , modifyVPCAttribute
    -- ** Request lenses
    , mvaEnableDNSHostnames
    , mvaEnableDNSSupport
    , mvaVPCId

    -- * Response
    , ModifyVPCAttributeResponse
    -- ** Response constructor
    , modifyVPCAttributeResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'modifyVPCAttribute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mvaEnableDNSHostnames'
--
-- * 'mvaEnableDNSSupport'
--
-- * 'mvaVPCId'
data ModifyVPCAttribute = ModifyVPCAttribute'
    { _mvaEnableDNSHostnames :: !(Maybe AttributeBooleanValue)
    , _mvaEnableDNSSupport   :: !(Maybe AttributeBooleanValue)
    , _mvaVPCId              :: !Text
    } deriving (Eq,Read,Show)

-- | 'ModifyVPCAttribute' smart constructor.
modifyVPCAttribute :: Text -> ModifyVPCAttribute
modifyVPCAttribute pVPCId =
    ModifyVPCAttribute'
    { _mvaEnableDNSHostnames = Nothing
    , _mvaEnableDNSSupport = Nothing
    , _mvaVPCId = pVPCId
    }

-- | Indicates whether the instances launched in the VPC get DNS hostnames.
-- If enabled, instances in the VPC get DNS hostnames; otherwise, they do
-- not.
--
-- You can only enable DNS hostnames if you also enable DNS support.
mvaEnableDNSHostnames :: Lens' ModifyVPCAttribute (Maybe AttributeBooleanValue)
mvaEnableDNSHostnames = lens _mvaEnableDNSHostnames (\ s a -> s{_mvaEnableDNSHostnames = a});

-- | Indicates whether the DNS resolution is supported for the VPC. If
-- enabled, queries to the Amazon provided DNS server at the
-- 169.254.169.253 IP address, or the reserved IP address at the base of
-- the VPC network range \"plus two\" will succeed. If disabled, the Amazon
-- provided DNS service in the VPC that resolves public DNS hostnames to IP
-- addresses is not enabled.
mvaEnableDNSSupport :: Lens' ModifyVPCAttribute (Maybe AttributeBooleanValue)
mvaEnableDNSSupport = lens _mvaEnableDNSSupport (\ s a -> s{_mvaEnableDNSSupport = a});

-- | The ID of the VPC.
mvaVPCId :: Lens' ModifyVPCAttribute Text
mvaVPCId = lens _mvaVPCId (\ s a -> s{_mvaVPCId = a});

instance AWSRequest ModifyVPCAttribute where
        type Sv ModifyVPCAttribute = EC2
        type Rs ModifyVPCAttribute =
             ModifyVPCAttributeResponse
        request = post
        response = receiveNull ModifyVPCAttributeResponse'

instance ToHeaders ModifyVPCAttribute where
        toHeaders = const mempty

instance ToPath ModifyVPCAttribute where
        toPath = const "/"

instance ToQuery ModifyVPCAttribute where
        toQuery ModifyVPCAttribute'{..}
          = mconcat
              ["Action" =: ("ModifyVPCAttribute" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "EnableDnsHostnames" =: _mvaEnableDNSHostnames,
               "EnableDnsSupport" =: _mvaEnableDNSSupport,
               "VpcId" =: _mvaVPCId]

-- | /See:/ 'modifyVPCAttributeResponse' smart constructor.
data ModifyVPCAttributeResponse =
    ModifyVPCAttributeResponse'
    deriving (Eq,Read,Show)

-- | 'ModifyVPCAttributeResponse' smart constructor.
modifyVPCAttributeResponse :: ModifyVPCAttributeResponse
modifyVPCAttributeResponse = ModifyVPCAttributeResponse'
