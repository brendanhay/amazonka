{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.Types.SingleMasterChannelEndpointConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideo.Types.SingleMasterChannelEndpointConfiguration
  ( SingleMasterChannelEndpointConfiguration (..),

    -- * Smart constructor
    mkSingleMasterChannelEndpointConfiguration,

    -- * Lenses
    smcecProtocols,
    smcecRole,
  )
where

import Network.AWS.KinesisVideo.Types.ChannelProtocol
import Network.AWS.KinesisVideo.Types.ChannelRole
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object that contains the endpoint configuration for the @SINGLE_MASTER@ channel type.
--
-- /See:/ 'mkSingleMasterChannelEndpointConfiguration' smart constructor.
data SingleMasterChannelEndpointConfiguration = SingleMasterChannelEndpointConfiguration'
  { -- | This property is used to determine the nature of communication over this @SINGLE_MASTER@ signaling channel. If @WSS@ is specified, this API returns a websocket endpoint. If @HTTPS@ is specified, this API returns an @HTTPS@ endpoint.
    protocols :: Lude.Maybe (Lude.NonEmpty ChannelProtocol),
    -- | This property is used to determine messaging permissions in this @SINGLE_MASTER@ signaling channel. If @MASTER@ is specified, this API returns an endpoint that a client can use to receive offers from and send answers to any of the viewers on this signaling channel. If @VIEWER@ is specified, this API returns an endpoint that a client can use only to send offers to another @MASTER@ client on this signaling channel.
    role' :: Lude.Maybe ChannelRole
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SingleMasterChannelEndpointConfiguration' with the minimum fields required to make a request.
--
-- * 'protocols' - This property is used to determine the nature of communication over this @SINGLE_MASTER@ signaling channel. If @WSS@ is specified, this API returns a websocket endpoint. If @HTTPS@ is specified, this API returns an @HTTPS@ endpoint.
-- * 'role'' - This property is used to determine messaging permissions in this @SINGLE_MASTER@ signaling channel. If @MASTER@ is specified, this API returns an endpoint that a client can use to receive offers from and send answers to any of the viewers on this signaling channel. If @VIEWER@ is specified, this API returns an endpoint that a client can use only to send offers to another @MASTER@ client on this signaling channel.
mkSingleMasterChannelEndpointConfiguration ::
  SingleMasterChannelEndpointConfiguration
mkSingleMasterChannelEndpointConfiguration =
  SingleMasterChannelEndpointConfiguration'
    { protocols =
        Lude.Nothing,
      role' = Lude.Nothing
    }

-- | This property is used to determine the nature of communication over this @SINGLE_MASTER@ signaling channel. If @WSS@ is specified, this API returns a websocket endpoint. If @HTTPS@ is specified, this API returns an @HTTPS@ endpoint.
--
-- /Note:/ Consider using 'protocols' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smcecProtocols :: Lens.Lens' SingleMasterChannelEndpointConfiguration (Lude.Maybe (Lude.NonEmpty ChannelProtocol))
smcecProtocols = Lens.lens (protocols :: SingleMasterChannelEndpointConfiguration -> Lude.Maybe (Lude.NonEmpty ChannelProtocol)) (\s a -> s {protocols = a} :: SingleMasterChannelEndpointConfiguration)
{-# DEPRECATED smcecProtocols "Use generic-lens or generic-optics with 'protocols' instead." #-}

-- | This property is used to determine messaging permissions in this @SINGLE_MASTER@ signaling channel. If @MASTER@ is specified, this API returns an endpoint that a client can use to receive offers from and send answers to any of the viewers on this signaling channel. If @VIEWER@ is specified, this API returns an endpoint that a client can use only to send offers to another @MASTER@ client on this signaling channel.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smcecRole :: Lens.Lens' SingleMasterChannelEndpointConfiguration (Lude.Maybe ChannelRole)
smcecRole = Lens.lens (role' :: SingleMasterChannelEndpointConfiguration -> Lude.Maybe ChannelRole) (\s a -> s {role' = a} :: SingleMasterChannelEndpointConfiguration)
{-# DEPRECATED smcecRole "Use generic-lens or generic-optics with 'role'' instead." #-}

instance Lude.ToJSON SingleMasterChannelEndpointConfiguration where
  toJSON SingleMasterChannelEndpointConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Protocols" Lude..=) Lude.<$> protocols,
            ("Role" Lude..=) Lude.<$> role'
          ]
      )
