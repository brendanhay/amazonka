{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyVPCAttribute
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified attribute of the specified VPC.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifyVPCAttribute.html AWS API Reference> for ModifyVPCAttribute.
module Network.AWS.EC2.ModifyVPCAttribute
    (
    -- * Creating a Request
      ModifyVPCAttribute
    , modifyVPCAttribute
    -- * Request Lenses
    , mvaEnableDNSHostnames
    , mvaEnableDNSSupport
    , mvaVPCId

    -- * Destructuring the Response
    , ModifyVPCAttributeResponse
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyVPCAttribute' smart constructor.
modifyVPCAttribute :: Text -> ModifyVPCAttribute
modifyVPCAttribute pVPCId_ =
    ModifyVPCAttribute'
    { _mvaEnableDNSHostnames = Nothing
    , _mvaEnableDNSSupport = Nothing
    , _mvaVPCId = pVPCId_
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
              ["Action" =: ("ModifyVpcAttribute" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "EnableDnsHostnames" =: _mvaEnableDNSHostnames,
               "EnableDnsSupport" =: _mvaEnableDNSSupport,
               "VpcId" =: _mvaVPCId]

-- | /See:/ 'modifyVPCAttributeResponse' smart constructor.
data ModifyVPCAttributeResponse =
    ModifyVPCAttributeResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyVPCAttributeResponse' smart constructor.
modifyVPCAttributeResponse :: ModifyVPCAttributeResponse
modifyVPCAttributeResponse = ModifyVPCAttributeResponse'
