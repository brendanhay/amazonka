{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.AllocateHostedConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a hosted connection on the specified interconnect or a link aggregation group (LAG) of interconnects.
--
-- Allocates a VLAN number and a specified amount of capacity (bandwidth) for use by a hosted connection on the specified interconnect or LAG of interconnects. AWS polices the hosted connection for the specified capacity and the AWS Direct Connect Partner must also police the hosted connection for the specified capacity.
module Network.AWS.DirectConnect.AllocateHostedConnection
  ( -- * Creating a request
    AllocateHostedConnection (..),
    mkAllocateHostedConnection,

    -- ** Request lenses
    ahcVlan,
    ahcConnectionId,
    ahcConnectionName,
    ahcBandwidth,
    ahcOwnerAccount,
    ahcTags,

    -- * Destructuring the response
    Connection (..),
    mkConnection,

    -- ** Response lenses
    cLagId,
    cVlan,
    cLocation,
    cAwsDevice,
    cHasLogicalRedundancy,
    cConnectionId,
    cLoaIssueTime,
    cPartnerName,
    cConnectionName,
    cBandwidth,
    cJumboFrameCapable,
    cOwnerAccount,
    cRegion,
    cProviderName,
    cAwsDeviceV2,
    cConnectionState,
    cTags,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAllocateHostedConnection' smart constructor.
data AllocateHostedConnection = AllocateHostedConnection'
  { -- | The dedicated VLAN provisioned to the hosted connection.
    vlan :: Lude.Int,
    -- | The ID of the interconnect or LAG.
    connectionId :: Lude.Text,
    -- | The name of the hosted connection.
    connectionName :: Lude.Text,
    -- | The bandwidth of the connection. The possible values are 50Mbps, 100Mbps, 200Mbps, 300Mbps, 400Mbps, 500Mbps, 1Gbps, 2Gbps, 5Gbps, and 10Gbps. Note that only those AWS Direct Connect Partners who have met specific requirements are allowed to create a 1Gbps, 2Gbps, 5Gbps or 10Gbps hosted connection.
    bandwidth :: Lude.Text,
    -- | The ID of the AWS account ID of the customer for the connection.
    ownerAccount :: Lude.Text,
    -- | The tags associated with the connection.
    tags :: Lude.Maybe (Lude.NonEmpty Tag)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AllocateHostedConnection' with the minimum fields required to make a request.
--
-- * 'vlan' - The dedicated VLAN provisioned to the hosted connection.
-- * 'connectionId' - The ID of the interconnect or LAG.
-- * 'connectionName' - The name of the hosted connection.
-- * 'bandwidth' - The bandwidth of the connection. The possible values are 50Mbps, 100Mbps, 200Mbps, 300Mbps, 400Mbps, 500Mbps, 1Gbps, 2Gbps, 5Gbps, and 10Gbps. Note that only those AWS Direct Connect Partners who have met specific requirements are allowed to create a 1Gbps, 2Gbps, 5Gbps or 10Gbps hosted connection.
-- * 'ownerAccount' - The ID of the AWS account ID of the customer for the connection.
-- * 'tags' - The tags associated with the connection.
mkAllocateHostedConnection ::
  -- | 'vlan'
  Lude.Int ->
  -- | 'connectionId'
  Lude.Text ->
  -- | 'connectionName'
  Lude.Text ->
  -- | 'bandwidth'
  Lude.Text ->
  -- | 'ownerAccount'
  Lude.Text ->
  AllocateHostedConnection
mkAllocateHostedConnection
  pVlan_
  pConnectionId_
  pConnectionName_
  pBandwidth_
  pOwnerAccount_ =
    AllocateHostedConnection'
      { vlan = pVlan_,
        connectionId = pConnectionId_,
        connectionName = pConnectionName_,
        bandwidth = pBandwidth_,
        ownerAccount = pOwnerAccount_,
        tags = Lude.Nothing
      }

-- | The dedicated VLAN provisioned to the hosted connection.
--
-- /Note:/ Consider using 'vlan' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahcVlan :: Lens.Lens' AllocateHostedConnection Lude.Int
ahcVlan = Lens.lens (vlan :: AllocateHostedConnection -> Lude.Int) (\s a -> s {vlan = a} :: AllocateHostedConnection)
{-# DEPRECATED ahcVlan "Use generic-lens or generic-optics with 'vlan' instead." #-}

-- | The ID of the interconnect or LAG.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahcConnectionId :: Lens.Lens' AllocateHostedConnection Lude.Text
ahcConnectionId = Lens.lens (connectionId :: AllocateHostedConnection -> Lude.Text) (\s a -> s {connectionId = a} :: AllocateHostedConnection)
{-# DEPRECATED ahcConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

-- | The name of the hosted connection.
--
-- /Note:/ Consider using 'connectionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahcConnectionName :: Lens.Lens' AllocateHostedConnection Lude.Text
ahcConnectionName = Lens.lens (connectionName :: AllocateHostedConnection -> Lude.Text) (\s a -> s {connectionName = a} :: AllocateHostedConnection)
{-# DEPRECATED ahcConnectionName "Use generic-lens or generic-optics with 'connectionName' instead." #-}

-- | The bandwidth of the connection. The possible values are 50Mbps, 100Mbps, 200Mbps, 300Mbps, 400Mbps, 500Mbps, 1Gbps, 2Gbps, 5Gbps, and 10Gbps. Note that only those AWS Direct Connect Partners who have met specific requirements are allowed to create a 1Gbps, 2Gbps, 5Gbps or 10Gbps hosted connection.
--
-- /Note:/ Consider using 'bandwidth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahcBandwidth :: Lens.Lens' AllocateHostedConnection Lude.Text
ahcBandwidth = Lens.lens (bandwidth :: AllocateHostedConnection -> Lude.Text) (\s a -> s {bandwidth = a} :: AllocateHostedConnection)
{-# DEPRECATED ahcBandwidth "Use generic-lens or generic-optics with 'bandwidth' instead." #-}

-- | The ID of the AWS account ID of the customer for the connection.
--
-- /Note:/ Consider using 'ownerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahcOwnerAccount :: Lens.Lens' AllocateHostedConnection Lude.Text
ahcOwnerAccount = Lens.lens (ownerAccount :: AllocateHostedConnection -> Lude.Text) (\s a -> s {ownerAccount = a} :: AllocateHostedConnection)
{-# DEPRECATED ahcOwnerAccount "Use generic-lens or generic-optics with 'ownerAccount' instead." #-}

-- | The tags associated with the connection.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahcTags :: Lens.Lens' AllocateHostedConnection (Lude.Maybe (Lude.NonEmpty Tag))
ahcTags = Lens.lens (tags :: AllocateHostedConnection -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: AllocateHostedConnection)
{-# DEPRECATED ahcTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest AllocateHostedConnection where
  type Rs AllocateHostedConnection = Connection
  request = Req.postJSON directConnectService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders AllocateHostedConnection where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OvertureService.AllocateHostedConnection" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AllocateHostedConnection where
  toJSON AllocateHostedConnection' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("vlan" Lude..= vlan),
            Lude.Just ("connectionId" Lude..= connectionId),
            Lude.Just ("connectionName" Lude..= connectionName),
            Lude.Just ("bandwidth" Lude..= bandwidth),
            Lude.Just ("ownerAccount" Lude..= ownerAccount),
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath AllocateHostedConnection where
  toPath = Lude.const "/"

instance Lude.ToQuery AllocateHostedConnection where
  toQuery = Lude.const Lude.mempty
