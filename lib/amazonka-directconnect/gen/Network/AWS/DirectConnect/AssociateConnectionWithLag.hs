{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.AssociateConnectionWithLag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an existing connection with a link aggregation group (LAG). The connection is interrupted and re-established as a member of the LAG (connectivity to AWS is interrupted). The connection must be hosted on the same AWS Direct Connect endpoint as the LAG, and its bandwidth must match the bandwidth for the LAG. You can re-associate a connection that's currently associated with a different LAG; however, if removing the connection would cause the original LAG to fall below its setting for minimum number of operational connections, the request fails.
--
-- Any virtual interfaces that are directly associated with the connection are automatically re-associated with the LAG. If the connection was originally associated with a different LAG, the virtual interfaces remain associated with the original LAG.
-- For interconnects, any hosted connections are automatically re-associated with the LAG. If the interconnect was originally associated with a different LAG, the hosted connections remain associated with the original LAG.
module Network.AWS.DirectConnect.AssociateConnectionWithLag
  ( -- * Creating a request
    AssociateConnectionWithLag (..),
    mkAssociateConnectionWithLag,

    -- ** Request lenses
    acwlLagId,
    acwlConnectionId,

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

-- | /See:/ 'mkAssociateConnectionWithLag' smart constructor.
data AssociateConnectionWithLag = AssociateConnectionWithLag'
  { -- | The ID of the LAG with which to associate the connection.
    lagId :: Lude.Text,
    -- | The ID of the connection.
    connectionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateConnectionWithLag' with the minimum fields required to make a request.
--
-- * 'lagId' - The ID of the LAG with which to associate the connection.
-- * 'connectionId' - The ID of the connection.
mkAssociateConnectionWithLag ::
  -- | 'lagId'
  Lude.Text ->
  -- | 'connectionId'
  Lude.Text ->
  AssociateConnectionWithLag
mkAssociateConnectionWithLag pLagId_ pConnectionId_ =
  AssociateConnectionWithLag'
    { lagId = pLagId_,
      connectionId = pConnectionId_
    }

-- | The ID of the LAG with which to associate the connection.
--
-- /Note:/ Consider using 'lagId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acwlLagId :: Lens.Lens' AssociateConnectionWithLag Lude.Text
acwlLagId = Lens.lens (lagId :: AssociateConnectionWithLag -> Lude.Text) (\s a -> s {lagId = a} :: AssociateConnectionWithLag)
{-# DEPRECATED acwlLagId "Use generic-lens or generic-optics with 'lagId' instead." #-}

-- | The ID of the connection.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acwlConnectionId :: Lens.Lens' AssociateConnectionWithLag Lude.Text
acwlConnectionId = Lens.lens (connectionId :: AssociateConnectionWithLag -> Lude.Text) (\s a -> s {connectionId = a} :: AssociateConnectionWithLag)
{-# DEPRECATED acwlConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

instance Lude.AWSRequest AssociateConnectionWithLag where
  type Rs AssociateConnectionWithLag = Connection
  request = Req.postJSON directConnectService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders AssociateConnectionWithLag where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OvertureService.AssociateConnectionWithLag" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateConnectionWithLag where
  toJSON AssociateConnectionWithLag' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("lagId" Lude..= lagId),
            Lude.Just ("connectionId" Lude..= connectionId)
          ]
      )

instance Lude.ToPath AssociateConnectionWithLag where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateConnectionWithLag where
  toQuery = Lude.const Lude.mempty
