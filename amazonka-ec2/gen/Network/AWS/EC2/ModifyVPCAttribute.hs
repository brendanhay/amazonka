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
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified attribute of the specified VPC.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifyVPCAttribute.html>
module Network.AWS.EC2.ModifyVPCAttribute
    (
    -- * Request
      ModifyVPCAttribute
    -- ** Request constructor
    , modifyVPCAttribute
    -- ** Request lenses
    , mvarqEnableDNSHostnames
    , mvarqEnableDNSSupport
    , mvarqVPCId

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
-- * 'mvarqEnableDNSHostnames'
--
-- * 'mvarqEnableDNSSupport'
--
-- * 'mvarqVPCId'
data ModifyVPCAttribute = ModifyVPCAttribute'
    { _mvarqEnableDNSHostnames :: !(Maybe AttributeBooleanValue)
    , _mvarqEnableDNSSupport   :: !(Maybe AttributeBooleanValue)
    , _mvarqVPCId              :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyVPCAttribute' smart constructor.
modifyVPCAttribute :: Text -> ModifyVPCAttribute
modifyVPCAttribute pVPCId =
    ModifyVPCAttribute'
    { _mvarqEnableDNSHostnames = Nothing
    , _mvarqEnableDNSSupport = Nothing
    , _mvarqVPCId = pVPCId
    }

-- | Indicates whether the instances launched in the VPC get DNS hostnames.
-- If enabled, instances in the VPC get DNS hostnames; otherwise, they do
-- not.
--
-- You can only enable DNS hostnames if you also enable DNS support.
mvarqEnableDNSHostnames :: Lens' ModifyVPCAttribute (Maybe AttributeBooleanValue)
mvarqEnableDNSHostnames = lens _mvarqEnableDNSHostnames (\ s a -> s{_mvarqEnableDNSHostnames = a});

-- | Indicates whether the DNS resolution is supported for the VPC. If
-- enabled, queries to the Amazon provided DNS server at the
-- 169.254.169.253 IP address, or the reserved IP address at the base of
-- the VPC network range \"plus two\" will succeed. If disabled, the Amazon
-- provided DNS service in the VPC that resolves public DNS hostnames to IP
-- addresses is not enabled.
mvarqEnableDNSSupport :: Lens' ModifyVPCAttribute (Maybe AttributeBooleanValue)
mvarqEnableDNSSupport = lens _mvarqEnableDNSSupport (\ s a -> s{_mvarqEnableDNSSupport = a});

-- | The ID of the VPC.
mvarqVPCId :: Lens' ModifyVPCAttribute Text
mvarqVPCId = lens _mvarqVPCId (\ s a -> s{_mvarqVPCId = a});

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
               "EnableDnsHostnames" =: _mvarqEnableDNSHostnames,
               "EnableDnsSupport" =: _mvarqEnableDNSSupport,
               "VpcId" =: _mvarqVPCId]

-- | /See:/ 'modifyVPCAttributeResponse' smart constructor.
data ModifyVPCAttributeResponse =
    ModifyVPCAttributeResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyVPCAttributeResponse' smart constructor.
modifyVPCAttributeResponse :: ModifyVPCAttributeResponse
modifyVPCAttributeResponse = ModifyVPCAttributeResponse'
