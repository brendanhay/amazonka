{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.AssociateHostedConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a hosted connection and its virtual interfaces with a link aggregation group (LAG) or interconnect. If the target interconnect or LAG has an existing hosted connection with a conflicting VLAN number or IP address, the operation fails. This action temporarily interrupts the hosted connection's connectivity to AWS as it is being migrated.
module Network.AWS.DirectConnect.AssociateHostedConnection
  ( -- * Creating a request
    AssociateHostedConnection (..),
    mkAssociateHostedConnection,

    -- ** Request lenses
    ahcfParentConnectionId,
    ahcfConnectionId,

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

-- | /See:/ 'mkAssociateHostedConnection' smart constructor.
data AssociateHostedConnection = AssociateHostedConnection'
  { -- | The ID of the interconnect or the LAG.
    parentConnectionId :: Lude.Text,
    -- | The ID of the hosted connection.
    connectionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateHostedConnection' with the minimum fields required to make a request.
--
-- * 'parentConnectionId' - The ID of the interconnect or the LAG.
-- * 'connectionId' - The ID of the hosted connection.
mkAssociateHostedConnection ::
  -- | 'parentConnectionId'
  Lude.Text ->
  -- | 'connectionId'
  Lude.Text ->
  AssociateHostedConnection
mkAssociateHostedConnection pParentConnectionId_ pConnectionId_ =
  AssociateHostedConnection'
    { parentConnectionId =
        pParentConnectionId_,
      connectionId = pConnectionId_
    }

-- | The ID of the interconnect or the LAG.
--
-- /Note:/ Consider using 'parentConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahcfParentConnectionId :: Lens.Lens' AssociateHostedConnection Lude.Text
ahcfParentConnectionId = Lens.lens (parentConnectionId :: AssociateHostedConnection -> Lude.Text) (\s a -> s {parentConnectionId = a} :: AssociateHostedConnection)
{-# DEPRECATED ahcfParentConnectionId "Use generic-lens or generic-optics with 'parentConnectionId' instead." #-}

-- | The ID of the hosted connection.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahcfConnectionId :: Lens.Lens' AssociateHostedConnection Lude.Text
ahcfConnectionId = Lens.lens (connectionId :: AssociateHostedConnection -> Lude.Text) (\s a -> s {connectionId = a} :: AssociateHostedConnection)
{-# DEPRECATED ahcfConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

instance Lude.AWSRequest AssociateHostedConnection where
  type Rs AssociateHostedConnection = Connection
  request = Req.postJSON directConnectService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders AssociateHostedConnection where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OvertureService.AssociateHostedConnection" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateHostedConnection where
  toJSON AssociateHostedConnection' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("parentConnectionId" Lude..= parentConnectionId),
            Lude.Just ("connectionId" Lude..= connectionId)
          ]
      )

instance Lude.ToPath AssociateHostedConnection where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateHostedConnection where
  toQuery = Lude.const Lude.mempty
