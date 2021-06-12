{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.AllocateHostedConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a hosted connection on the specified interconnect or a link
-- aggregation group (LAG) of interconnects.
--
-- Allocates a VLAN number and a specified amount of capacity (bandwidth)
-- for use by a hosted connection on the specified interconnect or LAG of
-- interconnects. AWS polices the hosted connection for the specified
-- capacity and the AWS Direct Connect Partner must also police the hosted
-- connection for the specified capacity.
--
-- Intended for use by AWS Direct Connect Partners only.
module Network.AWS.DirectConnect.AllocateHostedConnection
  ( -- * Creating a Request
    AllocateHostedConnection (..),
    newAllocateHostedConnection,

    -- * Request Lenses
    allocateHostedConnection_tags,
    allocateHostedConnection_connectionId,
    allocateHostedConnection_ownerAccount,
    allocateHostedConnection_bandwidth,
    allocateHostedConnection_connectionName,
    allocateHostedConnection_vlan,

    -- * Destructuring the Response
    Connection (..),
    newConnection,

    -- * Response Lenses
    connection_bandwidth,
    connection_connectionState,
    connection_awsDeviceV2,
    connection_connectionName,
    connection_providerName,
    connection_connectionId,
    connection_hasLogicalRedundancy,
    connection_awsDevice,
    connection_jumboFrameCapable,
    connection_lagId,
    connection_partnerName,
    connection_tags,
    connection_loaIssueTime,
    connection_ownerAccount,
    connection_region,
    connection_location,
    connection_vlan,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAllocateHostedConnection' smart constructor.
data AllocateHostedConnection = AllocateHostedConnection'
  { -- | The tags associated with the connection.
    tags :: Core.Maybe (Core.NonEmpty Tag),
    -- | The ID of the interconnect or LAG.
    connectionId :: Core.Text,
    -- | The ID of the AWS account ID of the customer for the connection.
    ownerAccount :: Core.Text,
    -- | The bandwidth of the connection. The possible values are 50Mbps,
    -- 100Mbps, 200Mbps, 300Mbps, 400Mbps, 500Mbps, 1Gbps, 2Gbps, 5Gbps, and
    -- 10Gbps. Note that only those AWS Direct Connect Partners who have met
    -- specific requirements are allowed to create a 1Gbps, 2Gbps, 5Gbps or
    -- 10Gbps hosted connection.
    bandwidth :: Core.Text,
    -- | The name of the hosted connection.
    connectionName :: Core.Text,
    -- | The dedicated VLAN provisioned to the hosted connection.
    vlan :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AllocateHostedConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'allocateHostedConnection_tags' - The tags associated with the connection.
--
-- 'connectionId', 'allocateHostedConnection_connectionId' - The ID of the interconnect or LAG.
--
-- 'ownerAccount', 'allocateHostedConnection_ownerAccount' - The ID of the AWS account ID of the customer for the connection.
--
-- 'bandwidth', 'allocateHostedConnection_bandwidth' - The bandwidth of the connection. The possible values are 50Mbps,
-- 100Mbps, 200Mbps, 300Mbps, 400Mbps, 500Mbps, 1Gbps, 2Gbps, 5Gbps, and
-- 10Gbps. Note that only those AWS Direct Connect Partners who have met
-- specific requirements are allowed to create a 1Gbps, 2Gbps, 5Gbps or
-- 10Gbps hosted connection.
--
-- 'connectionName', 'allocateHostedConnection_connectionName' - The name of the hosted connection.
--
-- 'vlan', 'allocateHostedConnection_vlan' - The dedicated VLAN provisioned to the hosted connection.
newAllocateHostedConnection ::
  -- | 'connectionId'
  Core.Text ->
  -- | 'ownerAccount'
  Core.Text ->
  -- | 'bandwidth'
  Core.Text ->
  -- | 'connectionName'
  Core.Text ->
  -- | 'vlan'
  Core.Int ->
  AllocateHostedConnection
newAllocateHostedConnection
  pConnectionId_
  pOwnerAccount_
  pBandwidth_
  pConnectionName_
  pVlan_ =
    AllocateHostedConnection'
      { tags = Core.Nothing,
        connectionId = pConnectionId_,
        ownerAccount = pOwnerAccount_,
        bandwidth = pBandwidth_,
        connectionName = pConnectionName_,
        vlan = pVlan_
      }

-- | The tags associated with the connection.
allocateHostedConnection_tags :: Lens.Lens' AllocateHostedConnection (Core.Maybe (Core.NonEmpty Tag))
allocateHostedConnection_tags = Lens.lens (\AllocateHostedConnection' {tags} -> tags) (\s@AllocateHostedConnection' {} a -> s {tags = a} :: AllocateHostedConnection) Core.. Lens.mapping Lens._Coerce

-- | The ID of the interconnect or LAG.
allocateHostedConnection_connectionId :: Lens.Lens' AllocateHostedConnection Core.Text
allocateHostedConnection_connectionId = Lens.lens (\AllocateHostedConnection' {connectionId} -> connectionId) (\s@AllocateHostedConnection' {} a -> s {connectionId = a} :: AllocateHostedConnection)

-- | The ID of the AWS account ID of the customer for the connection.
allocateHostedConnection_ownerAccount :: Lens.Lens' AllocateHostedConnection Core.Text
allocateHostedConnection_ownerAccount = Lens.lens (\AllocateHostedConnection' {ownerAccount} -> ownerAccount) (\s@AllocateHostedConnection' {} a -> s {ownerAccount = a} :: AllocateHostedConnection)

-- | The bandwidth of the connection. The possible values are 50Mbps,
-- 100Mbps, 200Mbps, 300Mbps, 400Mbps, 500Mbps, 1Gbps, 2Gbps, 5Gbps, and
-- 10Gbps. Note that only those AWS Direct Connect Partners who have met
-- specific requirements are allowed to create a 1Gbps, 2Gbps, 5Gbps or
-- 10Gbps hosted connection.
allocateHostedConnection_bandwidth :: Lens.Lens' AllocateHostedConnection Core.Text
allocateHostedConnection_bandwidth = Lens.lens (\AllocateHostedConnection' {bandwidth} -> bandwidth) (\s@AllocateHostedConnection' {} a -> s {bandwidth = a} :: AllocateHostedConnection)

-- | The name of the hosted connection.
allocateHostedConnection_connectionName :: Lens.Lens' AllocateHostedConnection Core.Text
allocateHostedConnection_connectionName = Lens.lens (\AllocateHostedConnection' {connectionName} -> connectionName) (\s@AllocateHostedConnection' {} a -> s {connectionName = a} :: AllocateHostedConnection)

-- | The dedicated VLAN provisioned to the hosted connection.
allocateHostedConnection_vlan :: Lens.Lens' AllocateHostedConnection Core.Int
allocateHostedConnection_vlan = Lens.lens (\AllocateHostedConnection' {vlan} -> vlan) (\s@AllocateHostedConnection' {} a -> s {vlan = a} :: AllocateHostedConnection)

instance Core.AWSRequest AllocateHostedConnection where
  type
    AWSResponse AllocateHostedConnection =
      Connection
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable AllocateHostedConnection

instance Core.NFData AllocateHostedConnection

instance Core.ToHeaders AllocateHostedConnection where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.AllocateHostedConnection" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AllocateHostedConnection where
  toJSON AllocateHostedConnection' {..} =
    Core.object
      ( Core.catMaybes
          [ ("tags" Core..=) Core.<$> tags,
            Core.Just ("connectionId" Core..= connectionId),
            Core.Just ("ownerAccount" Core..= ownerAccount),
            Core.Just ("bandwidth" Core..= bandwidth),
            Core.Just ("connectionName" Core..= connectionName),
            Core.Just ("vlan" Core..= vlan)
          ]
      )

instance Core.ToPath AllocateHostedConnection where
  toPath = Core.const "/"

instance Core.ToQuery AllocateHostedConnection where
  toQuery = Core.const Core.mempty
