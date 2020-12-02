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
-- Module      : Network.AWS.EC2.AllocateHosts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allocates a Dedicated Host to your account. At a minimum, specify the supported instance type or instance family, the Availability Zone in which to allocate the host, and the number of hosts to allocate.
module Network.AWS.EC2.AllocateHosts
  ( -- * Creating a Request
    allocateHosts,
    AllocateHosts,

    -- * Request Lenses
    ahInstanceFamily,
    ahClientToken,
    ahInstanceType,
    ahTagSpecifications,
    ahHostRecovery,
    ahAutoPlacement,
    ahAvailabilityZone,
    ahQuantity,

    -- * Destructuring the Response
    allocateHostsResponse,
    AllocateHostsResponse,

    -- * Response Lenses
    ahrsHostIds,
    ahrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'allocateHosts' smart constructor.
data AllocateHosts = AllocateHosts'
  { _ahInstanceFamily ::
      !(Maybe Text),
    _ahClientToken :: !(Maybe Text),
    _ahInstanceType :: !(Maybe Text),
    _ahTagSpecifications :: !(Maybe [TagSpecification]),
    _ahHostRecovery :: !(Maybe HostRecovery),
    _ahAutoPlacement :: !(Maybe AutoPlacement),
    _ahAvailabilityZone :: !Text,
    _ahQuantity :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AllocateHosts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ahInstanceFamily' - Specifies the instance family to be supported by the Dedicated Hosts. If you specify an instance family, the Dedicated Hosts support multiple instance types within that instance family. If you want the Dedicated Hosts to support a specific instance type only, omit this parameter and specify __InstanceType__ instead. You cannot specify __InstanceFamily__ and __InstanceType__ in the same request.
--
-- * 'ahClientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- * 'ahInstanceType' - Specifies the instance type to be supported by the Dedicated Hosts. If you specify an instance type, the Dedicated Hosts support instances of the specified instance type only. If you want the Dedicated Hosts to support multiple instance types in a specific instance family, omit this parameter and specify __InstanceFamily__ instead. You cannot specify __InstanceType__ and __InstanceFamily__ in the same request.
--
-- * 'ahTagSpecifications' - The tags to apply to the Dedicated Host during creation.
--
-- * 'ahHostRecovery' - Indicates whether to enable or disable host recovery for the Dedicated Host. Host recovery is disabled by default. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-hosts-recovery.html Host Recovery> in the /Amazon Elastic Compute Cloud User Guide/ . Default: @off@
--
-- * 'ahAutoPlacement' - Indicates whether the host accepts any untargeted instance launches that match its instance type configuration, or if it only accepts Host tenancy instance launches that specify its unique host ID. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/how-dedicated-hosts-work.html#dedicated-hosts-understanding Understanding Instance Placement and Host Affinity> in the /Amazon EC2 User Guide for Linux Instances/ . Default: @on@
--
-- * 'ahAvailabilityZone' - The Availability Zone in which to allocate the Dedicated Host.
--
-- * 'ahQuantity' - The number of Dedicated Hosts to allocate to your account with these parameters.
allocateHosts ::
  -- | 'ahAvailabilityZone'
  Text ->
  -- | 'ahQuantity'
  Int ->
  AllocateHosts
allocateHosts pAvailabilityZone_ pQuantity_ =
  AllocateHosts'
    { _ahInstanceFamily = Nothing,
      _ahClientToken = Nothing,
      _ahInstanceType = Nothing,
      _ahTagSpecifications = Nothing,
      _ahHostRecovery = Nothing,
      _ahAutoPlacement = Nothing,
      _ahAvailabilityZone = pAvailabilityZone_,
      _ahQuantity = pQuantity_
    }

-- | Specifies the instance family to be supported by the Dedicated Hosts. If you specify an instance family, the Dedicated Hosts support multiple instance types within that instance family. If you want the Dedicated Hosts to support a specific instance type only, omit this parameter and specify __InstanceType__ instead. You cannot specify __InstanceFamily__ and __InstanceType__ in the same request.
ahInstanceFamily :: Lens' AllocateHosts (Maybe Text)
ahInstanceFamily = lens _ahInstanceFamily (\s a -> s {_ahInstanceFamily = a})

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
ahClientToken :: Lens' AllocateHosts (Maybe Text)
ahClientToken = lens _ahClientToken (\s a -> s {_ahClientToken = a})

-- | Specifies the instance type to be supported by the Dedicated Hosts. If you specify an instance type, the Dedicated Hosts support instances of the specified instance type only. If you want the Dedicated Hosts to support multiple instance types in a specific instance family, omit this parameter and specify __InstanceFamily__ instead. You cannot specify __InstanceType__ and __InstanceFamily__ in the same request.
ahInstanceType :: Lens' AllocateHosts (Maybe Text)
ahInstanceType = lens _ahInstanceType (\s a -> s {_ahInstanceType = a})

-- | The tags to apply to the Dedicated Host during creation.
ahTagSpecifications :: Lens' AllocateHosts [TagSpecification]
ahTagSpecifications = lens _ahTagSpecifications (\s a -> s {_ahTagSpecifications = a}) . _Default . _Coerce

-- | Indicates whether to enable or disable host recovery for the Dedicated Host. Host recovery is disabled by default. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-hosts-recovery.html Host Recovery> in the /Amazon Elastic Compute Cloud User Guide/ . Default: @off@
ahHostRecovery :: Lens' AllocateHosts (Maybe HostRecovery)
ahHostRecovery = lens _ahHostRecovery (\s a -> s {_ahHostRecovery = a})

-- | Indicates whether the host accepts any untargeted instance launches that match its instance type configuration, or if it only accepts Host tenancy instance launches that specify its unique host ID. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/how-dedicated-hosts-work.html#dedicated-hosts-understanding Understanding Instance Placement and Host Affinity> in the /Amazon EC2 User Guide for Linux Instances/ . Default: @on@
ahAutoPlacement :: Lens' AllocateHosts (Maybe AutoPlacement)
ahAutoPlacement = lens _ahAutoPlacement (\s a -> s {_ahAutoPlacement = a})

-- | The Availability Zone in which to allocate the Dedicated Host.
ahAvailabilityZone :: Lens' AllocateHosts Text
ahAvailabilityZone = lens _ahAvailabilityZone (\s a -> s {_ahAvailabilityZone = a})

-- | The number of Dedicated Hosts to allocate to your account with these parameters.
ahQuantity :: Lens' AllocateHosts Int
ahQuantity = lens _ahQuantity (\s a -> s {_ahQuantity = a})

instance AWSRequest AllocateHosts where
  type Rs AllocateHosts = AllocateHostsResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          AllocateHostsResponse'
            <$> (x .@? "hostIdSet" .!@ mempty >>= may (parseXMLList "item"))
            <*> (pure (fromEnum s))
      )

instance Hashable AllocateHosts

instance NFData AllocateHosts

instance ToHeaders AllocateHosts where
  toHeaders = const mempty

instance ToPath AllocateHosts where
  toPath = const "/"

instance ToQuery AllocateHosts where
  toQuery AllocateHosts' {..} =
    mconcat
      [ "Action" =: ("AllocateHosts" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "InstanceFamily" =: _ahInstanceFamily,
        "ClientToken" =: _ahClientToken,
        "InstanceType" =: _ahInstanceType,
        toQuery (toQueryList "TagSpecification" <$> _ahTagSpecifications),
        "HostRecovery" =: _ahHostRecovery,
        "AutoPlacement" =: _ahAutoPlacement,
        "AvailabilityZone" =: _ahAvailabilityZone,
        "Quantity" =: _ahQuantity
      ]

-- | Contains the output of AllocateHosts.
--
--
--
-- /See:/ 'allocateHostsResponse' smart constructor.
data AllocateHostsResponse = AllocateHostsResponse'
  { _ahrsHostIds ::
      !(Maybe [Text]),
    _ahrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AllocateHostsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ahrsHostIds' - The ID of the allocated Dedicated Host. This is used to launch an instance onto a specific host.
--
-- * 'ahrsResponseStatus' - -- | The response status code.
allocateHostsResponse ::
  -- | 'ahrsResponseStatus'
  Int ->
  AllocateHostsResponse
allocateHostsResponse pResponseStatus_ =
  AllocateHostsResponse'
    { _ahrsHostIds = Nothing,
      _ahrsResponseStatus = pResponseStatus_
    }

-- | The ID of the allocated Dedicated Host. This is used to launch an instance onto a specific host.
ahrsHostIds :: Lens' AllocateHostsResponse [Text]
ahrsHostIds = lens _ahrsHostIds (\s a -> s {_ahrsHostIds = a}) . _Default . _Coerce

-- | -- | The response status code.
ahrsResponseStatus :: Lens' AllocateHostsResponse Int
ahrsResponseStatus = lens _ahrsResponseStatus (\s a -> s {_ahrsResponseStatus = a})

instance NFData AllocateHostsResponse
