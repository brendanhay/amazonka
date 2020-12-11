{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.UpdateLag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the attributes of the specified link aggregation group (LAG).
--
-- You can update the following attributes:
--
--     * The name of the LAG.
--
--
--     * The value for the minimum number of connections that must be operational for the LAG itself to be operational.
--
--
-- When you create a LAG, the default value for the minimum number of operational connections is zero (0). If you update this value and the number of operational connections falls below the specified value, the LAG automatically goes down to avoid over-utilization of the remaining connections. Adjust this value with care, as it could force the LAG down if it is set higher than the current number of operational connections.
module Network.AWS.DirectConnect.UpdateLag
  ( -- * Creating a request
    UpdateLag (..),
    mkUpdateLag,

    -- ** Request lenses
    ulMinimumLinks,
    ulLagName,
    ulLagId,

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

-- | /See:/ 'mkUpdateLag' smart constructor.
data UpdateLag = UpdateLag'
  { minimumLinks :: Lude.Maybe Lude.Int,
    lagName :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'UpdateLag' with the minimum fields required to make a request.
--
-- * 'lagId' - The ID of the LAG.
-- * 'lagName' - The name of the LAG.
-- * 'minimumLinks' - The minimum number of physical connections that must be operational for the LAG itself to be operational.
mkUpdateLag ::
  -- | 'lagId'
  Lude.Text ->
  UpdateLag
mkUpdateLag pLagId_ =
  UpdateLag'
    { minimumLinks = Lude.Nothing,
      lagName = Lude.Nothing,
      lagId = pLagId_
    }

-- | The minimum number of physical connections that must be operational for the LAG itself to be operational.
--
-- /Note:/ Consider using 'minimumLinks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulMinimumLinks :: Lens.Lens' UpdateLag (Lude.Maybe Lude.Int)
ulMinimumLinks = Lens.lens (minimumLinks :: UpdateLag -> Lude.Maybe Lude.Int) (\s a -> s {minimumLinks = a} :: UpdateLag)
{-# DEPRECATED ulMinimumLinks "Use generic-lens or generic-optics with 'minimumLinks' instead." #-}

-- | The name of the LAG.
--
-- /Note:/ Consider using 'lagName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulLagName :: Lens.Lens' UpdateLag (Lude.Maybe Lude.Text)
ulLagName = Lens.lens (lagName :: UpdateLag -> Lude.Maybe Lude.Text) (\s a -> s {lagName = a} :: UpdateLag)
{-# DEPRECATED ulLagName "Use generic-lens or generic-optics with 'lagName' instead." #-}

-- | The ID of the LAG.
--
-- /Note:/ Consider using 'lagId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulLagId :: Lens.Lens' UpdateLag Lude.Text
ulLagId = Lens.lens (lagId :: UpdateLag -> Lude.Text) (\s a -> s {lagId = a} :: UpdateLag)
{-# DEPRECATED ulLagId "Use generic-lens or generic-optics with 'lagId' instead." #-}

instance Lude.AWSRequest UpdateLag where
  type Rs UpdateLag = Lag
  request = Req.postJSON directConnectService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders UpdateLag where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OvertureService.UpdateLag" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateLag where
  toJSON UpdateLag' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("minimumLinks" Lude..=) Lude.<$> minimumLinks,
            ("lagName" Lude..=) Lude.<$> lagName,
            Lude.Just ("lagId" Lude..= lagId)
          ]
      )

instance Lude.ToPath UpdateLag where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateLag where
  toQuery = Lude.const Lude.mempty
