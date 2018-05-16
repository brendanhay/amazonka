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
-- Module      : Network.AWS.EC2.ModifyIdFormat
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the ID format for the specified resource on a per-region basis. You can specify that resources should receive longer IDs (17-character IDs) when they are created.
--
--
-- This request can only be used to modify longer ID settings for resource types that are within the opt-in period. Resources currently in their opt-in period include: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @route-table@ | @route-table-association@ | @security-group@ | @subnet@ | @subnet-cidr-block-association@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@ .
--
-- This setting applies to the IAM user who makes the request; it does not apply to the entire AWS account. By default, an IAM user defaults to the same settings as the root user. If you're using this action as the root user, then these settings apply to the entire account, unless an IAM user explicitly overrides these settings for themselves. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/resource-ids.html Resource IDs> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- Resources created with longer IDs are visible to all IAM roles and users, regardless of these settings and provided that they have permission to use the relevant @Describe@ command for the resource type.
--
module Network.AWS.EC2.ModifyIdFormat
    (
    -- * Creating a Request
      modifyIdFormat
    , ModifyIdFormat
    -- * Request Lenses
    , mifResource
    , mifUseLongIds

    -- * Destructuring the Response
    , modifyIdFormatResponse
    , ModifyIdFormatResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters of ModifyIdFormat.
--
--
--
-- /See:/ 'modifyIdFormat' smart constructor.
data ModifyIdFormat = ModifyIdFormat'
  { _mifResource   :: !Text
  , _mifUseLongIds :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyIdFormat' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mifResource' - The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @route-table@ | @route-table-association@ | @security-group@ | @subnet@ | @subnet-cidr-block-association@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@ . Alternatively, use the @all-current@ option to include all resource types that are currently within their opt-in period for longer IDs.
--
-- * 'mifUseLongIds' - Indicate whether the resource should use longer IDs (17-character IDs).
modifyIdFormat
    :: Text -- ^ 'mifResource'
    -> Bool -- ^ 'mifUseLongIds'
    -> ModifyIdFormat
modifyIdFormat pResource_ pUseLongIds_ =
  ModifyIdFormat' {_mifResource = pResource_, _mifUseLongIds = pUseLongIds_}


-- | The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @route-table@ | @route-table-association@ | @security-group@ | @subnet@ | @subnet-cidr-block-association@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@ . Alternatively, use the @all-current@ option to include all resource types that are currently within their opt-in period for longer IDs.
mifResource :: Lens' ModifyIdFormat Text
mifResource = lens _mifResource (\ s a -> s{_mifResource = a})

-- | Indicate whether the resource should use longer IDs (17-character IDs).
mifUseLongIds :: Lens' ModifyIdFormat Bool
mifUseLongIds = lens _mifUseLongIds (\ s a -> s{_mifUseLongIds = a})

instance AWSRequest ModifyIdFormat where
        type Rs ModifyIdFormat = ModifyIdFormatResponse
        request = postQuery ec2
        response = receiveNull ModifyIdFormatResponse'

instance Hashable ModifyIdFormat where

instance NFData ModifyIdFormat where

instance ToHeaders ModifyIdFormat where
        toHeaders = const mempty

instance ToPath ModifyIdFormat where
        toPath = const "/"

instance ToQuery ModifyIdFormat where
        toQuery ModifyIdFormat'{..}
          = mconcat
              ["Action" =: ("ModifyIdFormat" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "Resource" =: _mifResource,
               "UseLongIds" =: _mifUseLongIds]

-- | /See:/ 'modifyIdFormatResponse' smart constructor.
data ModifyIdFormatResponse =
  ModifyIdFormatResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyIdFormatResponse' with the minimum fields required to make a request.
--
modifyIdFormatResponse
    :: ModifyIdFormatResponse
modifyIdFormatResponse = ModifyIdFormatResponse'


instance NFData ModifyIdFormatResponse where
