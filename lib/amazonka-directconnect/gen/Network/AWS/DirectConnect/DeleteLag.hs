{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DeleteLag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified link aggregation group (LAG). You cannot delete a LAG if it has active virtual interfaces or hosted connections.
module Network.AWS.DirectConnect.DeleteLag
  ( -- * Creating a request
    DeleteLag (..),
    mkDeleteLag,

    -- ** Request lenses
    dLagId,

    -- * Destructuring the response
    Lag (..),
    mkLag,

    -- ** Response lenses
    lagLagId,
    lagConnectionsBandwidth,
    lagMinimumLinks,
    lagLagName,
    lagLocation,
    lagConnections,
    lagAwsDevice,
    lagHasLogicalRedundancy,
    lagAllowsHostedConnections,
    lagNumberOfConnections,
    lagJumboFrameCapable,
    lagLagState,
    lagOwnerAccount,
    lagRegion,
    lagProviderName,
    lagAwsDeviceV2,
    lagTags,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteLag' smart constructor.
newtype DeleteLag = DeleteLag' {lagId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLag' with the minimum fields required to make a request.
--
-- * 'lagId' - The ID of the LAG.
mkDeleteLag ::
  -- | 'lagId'
  Lude.Text ->
  DeleteLag
mkDeleteLag pLagId_ = DeleteLag' {lagId = pLagId_}

-- | The ID of the LAG.
--
-- /Note:/ Consider using 'lagId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLagId :: Lens.Lens' DeleteLag Lude.Text
dLagId = Lens.lens (lagId :: DeleteLag -> Lude.Text) (\s a -> s {lagId = a} :: DeleteLag)
{-# DEPRECATED dLagId "Use generic-lens or generic-optics with 'lagId' instead." #-}

instance Lude.AWSRequest DeleteLag where
  type Rs DeleteLag = Lag
  request = Req.postJSON directConnectService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders DeleteLag where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OvertureService.DeleteLag" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteLag where
  toJSON DeleteLag' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("lagId" Lude..= lagId)])

instance Lude.ToPath DeleteLag where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteLag where
  toQuery = Lude.const Lude.mempty
