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
-- Module      : Network.AWS.EC2.ModifyVPCAttribute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified attribute of the specified VPC.
--
--
module Network.AWS.EC2.ModifyVPCAttribute
    (
    -- * Creating a Request
      modifyVPCAttribute
    , ModifyVPCAttribute
    -- * Request Lenses
    , mvaEnableDNSHostnames
    , mvaEnableDNSSupport
    , mvaVPCId

    -- * Destructuring the Response
    , modifyVPCAttributeResponse
    , ModifyVPCAttributeResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyVPCAttribute' smart constructor.
data ModifyVPCAttribute = ModifyVPCAttribute'
  { _mvaEnableDNSHostnames :: !(Maybe AttributeBooleanValue)
  , _mvaEnableDNSSupport   :: !(Maybe AttributeBooleanValue)
  , _mvaVPCId              :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyVPCAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mvaEnableDNSHostnames' - Indicates whether the instances launched in the VPC get DNS hostnames. If enabled, instances in the VPC get DNS hostnames; otherwise, they do not. You cannot modify the DNS resolution and DNS hostnames attributes in the same request. Use separate requests for each attribute. You can only enable DNS hostnames if you've enabled DNS support.
--
-- * 'mvaEnableDNSSupport' - Indicates whether the DNS resolution is supported for the VPC. If enabled, queries to the Amazon provided DNS server at the 169.254.169.253 IP address, or the reserved IP address at the base of the VPC network range "plus two" succeed. If disabled, the Amazon provided DNS service in the VPC that resolves public DNS hostnames to IP addresses is not enabled. You cannot modify the DNS resolution and DNS hostnames attributes in the same request. Use separate requests for each attribute.
--
-- * 'mvaVPCId' - The ID of the VPC.
modifyVPCAttribute
    :: Text -- ^ 'mvaVPCId'
    -> ModifyVPCAttribute
modifyVPCAttribute pVPCId_ =
  ModifyVPCAttribute'
    { _mvaEnableDNSHostnames = Nothing
    , _mvaEnableDNSSupport = Nothing
    , _mvaVPCId = pVPCId_
    }


-- | Indicates whether the instances launched in the VPC get DNS hostnames. If enabled, instances in the VPC get DNS hostnames; otherwise, they do not. You cannot modify the DNS resolution and DNS hostnames attributes in the same request. Use separate requests for each attribute. You can only enable DNS hostnames if you've enabled DNS support.
mvaEnableDNSHostnames :: Lens' ModifyVPCAttribute (Maybe AttributeBooleanValue)
mvaEnableDNSHostnames = lens _mvaEnableDNSHostnames (\ s a -> s{_mvaEnableDNSHostnames = a})

-- | Indicates whether the DNS resolution is supported for the VPC. If enabled, queries to the Amazon provided DNS server at the 169.254.169.253 IP address, or the reserved IP address at the base of the VPC network range "plus two" succeed. If disabled, the Amazon provided DNS service in the VPC that resolves public DNS hostnames to IP addresses is not enabled. You cannot modify the DNS resolution and DNS hostnames attributes in the same request. Use separate requests for each attribute.
mvaEnableDNSSupport :: Lens' ModifyVPCAttribute (Maybe AttributeBooleanValue)
mvaEnableDNSSupport = lens _mvaEnableDNSSupport (\ s a -> s{_mvaEnableDNSSupport = a})

-- | The ID of the VPC.
mvaVPCId :: Lens' ModifyVPCAttribute Text
mvaVPCId = lens _mvaVPCId (\ s a -> s{_mvaVPCId = a})

instance AWSRequest ModifyVPCAttribute where
        type Rs ModifyVPCAttribute =
             ModifyVPCAttributeResponse
        request = postQuery ec2
        response = receiveNull ModifyVPCAttributeResponse'

instance Hashable ModifyVPCAttribute where

instance NFData ModifyVPCAttribute where

instance ToHeaders ModifyVPCAttribute where
        toHeaders = const mempty

instance ToPath ModifyVPCAttribute where
        toPath = const "/"

instance ToQuery ModifyVPCAttribute where
        toQuery ModifyVPCAttribute'{..}
          = mconcat
              ["Action" =: ("ModifyVpcAttribute" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "EnableDnsHostnames" =: _mvaEnableDNSHostnames,
               "EnableDnsSupport" =: _mvaEnableDNSSupport,
               "VpcId" =: _mvaVPCId]

-- | /See:/ 'modifyVPCAttributeResponse' smart constructor.
data ModifyVPCAttributeResponse =
  ModifyVPCAttributeResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyVPCAttributeResponse' with the minimum fields required to make a request.
--
modifyVPCAttributeResponse
    :: ModifyVPCAttributeResponse
modifyVPCAttributeResponse = ModifyVPCAttributeResponse'


instance NFData ModifyVPCAttributeResponse where
