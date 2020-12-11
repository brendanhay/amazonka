{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DisassociateConnectionFromLag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a connection from a link aggregation group (LAG). The connection is interrupted and re-established as a standalone connection (the connection is not deleted; to delete the connection, use the 'DeleteConnection' request). If the LAG has associated virtual interfaces or hosted connections, they remain associated with the LAG. A disassociated connection owned by an AWS Direct Connect Partner is automatically converted to an interconnect.
--
-- If disassociating the connection would cause the LAG to fall below its setting for minimum number of operational connections, the request fails, except when it's the last member of the LAG. If all connections are disassociated, the LAG continues to exist as an empty LAG with no physical connections.
module Network.AWS.DirectConnect.DisassociateConnectionFromLag
  ( -- * Creating a request
    DisassociateConnectionFromLag (..),
    mkDisassociateConnectionFromLag,

    -- ** Request lenses
    dcflConnectionId,
    dcflLagId,

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

-- | /See:/ 'mkDisassociateConnectionFromLag' smart constructor.
data DisassociateConnectionFromLag = DisassociateConnectionFromLag'
  { connectionId ::
      Lude.Text,
    lagId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateConnectionFromLag' with the minimum fields required to make a request.
--
-- * 'connectionId' - The ID of the connection.
-- * 'lagId' - The ID of the LAG.
mkDisassociateConnectionFromLag ::
  -- | 'connectionId'
  Lude.Text ->
  -- | 'lagId'
  Lude.Text ->
  DisassociateConnectionFromLag
mkDisassociateConnectionFromLag pConnectionId_ pLagId_ =
  DisassociateConnectionFromLag'
    { connectionId = pConnectionId_,
      lagId = pLagId_
    }

-- | The ID of the connection.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcflConnectionId :: Lens.Lens' DisassociateConnectionFromLag Lude.Text
dcflConnectionId = Lens.lens (connectionId :: DisassociateConnectionFromLag -> Lude.Text) (\s a -> s {connectionId = a} :: DisassociateConnectionFromLag)
{-# DEPRECATED dcflConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

-- | The ID of the LAG.
--
-- /Note:/ Consider using 'lagId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcflLagId :: Lens.Lens' DisassociateConnectionFromLag Lude.Text
dcflLagId = Lens.lens (lagId :: DisassociateConnectionFromLag -> Lude.Text) (\s a -> s {lagId = a} :: DisassociateConnectionFromLag)
{-# DEPRECATED dcflLagId "Use generic-lens or generic-optics with 'lagId' instead." #-}

instance Lude.AWSRequest DisassociateConnectionFromLag where
  type Rs DisassociateConnectionFromLag = Connection
  request = Req.postJSON directConnectService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders DisassociateConnectionFromLag where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "OvertureService.DisassociateConnectionFromLag" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisassociateConnectionFromLag where
  toJSON DisassociateConnectionFromLag' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("connectionId" Lude..= connectionId),
            Lude.Just ("lagId" Lude..= lagId)
          ]
      )

instance Lude.ToPath DisassociateConnectionFromLag where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateConnectionFromLag where
  toQuery = Lude.const Lude.mempty
