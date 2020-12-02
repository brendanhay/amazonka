{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyHosts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify the auto-placement setting of a Dedicated Host. When auto-placement is enabled, any instances that you launch with a tenancy of @host@ but without a specific host ID are placed onto any available Dedicated Host in your account that has auto-placement enabled. When auto-placement is disabled, you need to provide a host ID to have the instance launch onto a specific host. If no host ID is provided, the instance is launched onto a suitable host with auto-placement enabled.
--
--
-- You can also use this API action to modify a Dedicated Host to support either multiple instance types in an instance family, or to support a specific instance type only.
module Network.AWS.EC2.ModifyHosts
  ( -- * Creating a Request
    modifyHosts,
    ModifyHosts,

    -- * Request Lenses
    mhInstanceFamily,
    mhInstanceType,
    mhHostRecovery,
    mhAutoPlacement,
    mhHostIds,

    -- * Destructuring the Response
    modifyHostsResponse,
    ModifyHostsResponse,

    -- * Response Lenses
    mhrsUnsuccessful,
    mhrsSuccessful,
    mhrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyHosts' smart constructor.
data ModifyHosts = ModifyHosts'
  { _mhInstanceFamily :: !(Maybe Text),
    _mhInstanceType :: !(Maybe Text),
    _mhHostRecovery :: !(Maybe HostRecovery),
    _mhAutoPlacement :: !(Maybe AutoPlacement),
    _mhHostIds :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyHosts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mhInstanceFamily' - Specifies the instance family to be supported by the Dedicated Host. Specify this parameter to modify a Dedicated Host to support multiple instance types within its current instance family. If you want to modify a Dedicated Host to support a specific instance type only, omit this parameter and specify __InstanceType__ instead. You cannot specify __InstanceFamily__ and __InstanceType__ in the same request.
--
-- * 'mhInstanceType' - Specifies the instance type to be supported by the Dedicated Host. Specify this parameter to modify a Dedicated Host to support only a specific instance type. If you want to modify a Dedicated Host to support multiple instance types in its current instance family, omit this parameter and specify __InstanceFamily__ instead. You cannot specify __InstanceType__ and __InstanceFamily__ in the same request.
--
-- * 'mhHostRecovery' - Indicates whether to enable or disable host recovery for the Dedicated Host. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-hosts-recovery.html Host Recovery> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'mhAutoPlacement' - Specify whether to enable or disable auto-placement.
--
-- * 'mhHostIds' - The IDs of the Dedicated Hosts to modify.
modifyHosts ::
  ModifyHosts
modifyHosts =
  ModifyHosts'
    { _mhInstanceFamily = Nothing,
      _mhInstanceType = Nothing,
      _mhHostRecovery = Nothing,
      _mhAutoPlacement = Nothing,
      _mhHostIds = mempty
    }

-- | Specifies the instance family to be supported by the Dedicated Host. Specify this parameter to modify a Dedicated Host to support multiple instance types within its current instance family. If you want to modify a Dedicated Host to support a specific instance type only, omit this parameter and specify __InstanceType__ instead. You cannot specify __InstanceFamily__ and __InstanceType__ in the same request.
mhInstanceFamily :: Lens' ModifyHosts (Maybe Text)
mhInstanceFamily = lens _mhInstanceFamily (\s a -> s {_mhInstanceFamily = a})

-- | Specifies the instance type to be supported by the Dedicated Host. Specify this parameter to modify a Dedicated Host to support only a specific instance type. If you want to modify a Dedicated Host to support multiple instance types in its current instance family, omit this parameter and specify __InstanceFamily__ instead. You cannot specify __InstanceType__ and __InstanceFamily__ in the same request.
mhInstanceType :: Lens' ModifyHosts (Maybe Text)
mhInstanceType = lens _mhInstanceType (\s a -> s {_mhInstanceType = a})

-- | Indicates whether to enable or disable host recovery for the Dedicated Host. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-hosts-recovery.html Host Recovery> in the /Amazon Elastic Compute Cloud User Guide/ .
mhHostRecovery :: Lens' ModifyHosts (Maybe HostRecovery)
mhHostRecovery = lens _mhHostRecovery (\s a -> s {_mhHostRecovery = a})

-- | Specify whether to enable or disable auto-placement.
mhAutoPlacement :: Lens' ModifyHosts (Maybe AutoPlacement)
mhAutoPlacement = lens _mhAutoPlacement (\s a -> s {_mhAutoPlacement = a})

-- | The IDs of the Dedicated Hosts to modify.
mhHostIds :: Lens' ModifyHosts [Text]
mhHostIds = lens _mhHostIds (\s a -> s {_mhHostIds = a}) . _Coerce

instance AWSRequest ModifyHosts where
  type Rs ModifyHosts = ModifyHostsResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          ModifyHostsResponse'
            <$> (x .@? "unsuccessful" .!@ mempty >>= may (parseXMLList "item"))
            <*> (x .@? "successful" .!@ mempty >>= may (parseXMLList "item"))
            <*> (pure (fromEnum s))
      )

instance Hashable ModifyHosts

instance NFData ModifyHosts

instance ToHeaders ModifyHosts where
  toHeaders = const mempty

instance ToPath ModifyHosts where
  toPath = const "/"

instance ToQuery ModifyHosts where
  toQuery ModifyHosts' {..} =
    mconcat
      [ "Action" =: ("ModifyHosts" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "InstanceFamily" =: _mhInstanceFamily,
        "InstanceType" =: _mhInstanceType,
        "HostRecovery" =: _mhHostRecovery,
        "AutoPlacement" =: _mhAutoPlacement,
        toQueryList "HostId" _mhHostIds
      ]

-- | /See:/ 'modifyHostsResponse' smart constructor.
data ModifyHostsResponse = ModifyHostsResponse'
  { _mhrsUnsuccessful ::
      !(Maybe [UnsuccessfulItem]),
    _mhrsSuccessful :: !(Maybe [Text]),
    _mhrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyHostsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mhrsUnsuccessful' - The IDs of the Dedicated Hosts that could not be modified. Check whether the setting you requested can be used.
--
-- * 'mhrsSuccessful' - The IDs of the Dedicated Hosts that were successfully modified.
--
-- * 'mhrsResponseStatus' - -- | The response status code.
modifyHostsResponse ::
  -- | 'mhrsResponseStatus'
  Int ->
  ModifyHostsResponse
modifyHostsResponse pResponseStatus_ =
  ModifyHostsResponse'
    { _mhrsUnsuccessful = Nothing,
      _mhrsSuccessful = Nothing,
      _mhrsResponseStatus = pResponseStatus_
    }

-- | The IDs of the Dedicated Hosts that could not be modified. Check whether the setting you requested can be used.
mhrsUnsuccessful :: Lens' ModifyHostsResponse [UnsuccessfulItem]
mhrsUnsuccessful = lens _mhrsUnsuccessful (\s a -> s {_mhrsUnsuccessful = a}) . _Default . _Coerce

-- | The IDs of the Dedicated Hosts that were successfully modified.
mhrsSuccessful :: Lens' ModifyHostsResponse [Text]
mhrsSuccessful = lens _mhrsSuccessful (\s a -> s {_mhrsSuccessful = a}) . _Default . _Coerce

-- | -- | The response status code.
mhrsResponseStatus :: Lens' ModifyHostsResponse Int
mhrsResponseStatus = lens _mhrsResponseStatus (\s a -> s {_mhrsResponseStatus = a})

instance NFData ModifyHostsResponse
