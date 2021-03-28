{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.Types.SingleMasterChannelEndpointConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisVideo.Types.SingleMasterChannelEndpointConfiguration
  ( SingleMasterChannelEndpointConfiguration (..)
  -- * Smart constructor
  , mkSingleMasterChannelEndpointConfiguration
  -- * Lenses
  , smcecProtocols
  , smcecRole
  ) where

import qualified Network.AWS.KinesisVideo.Types.ChannelProtocol as Types
import qualified Network.AWS.KinesisVideo.Types.ChannelRole as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object that contains the endpoint configuration for the @SINGLE_MASTER@ channel type. 
--
-- /See:/ 'mkSingleMasterChannelEndpointConfiguration' smart constructor.
data SingleMasterChannelEndpointConfiguration = SingleMasterChannelEndpointConfiguration'
  { protocols :: Core.Maybe (Core.NonEmpty Types.ChannelProtocol)
    -- ^ This property is used to determine the nature of communication over this @SINGLE_MASTER@ signaling channel. If @WSS@ is specified, this API returns a websocket endpoint. If @HTTPS@ is specified, this API returns an @HTTPS@ endpoint.
  , role' :: Core.Maybe Types.ChannelRole
    -- ^ This property is used to determine messaging permissions in this @SINGLE_MASTER@ signaling channel. If @MASTER@ is specified, this API returns an endpoint that a client can use to receive offers from and send answers to any of the viewers on this signaling channel. If @VIEWER@ is specified, this API returns an endpoint that a client can use only to send offers to another @MASTER@ client on this signaling channel. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SingleMasterChannelEndpointConfiguration' value with any optional fields omitted.
mkSingleMasterChannelEndpointConfiguration
    :: SingleMasterChannelEndpointConfiguration
mkSingleMasterChannelEndpointConfiguration
  = SingleMasterChannelEndpointConfiguration'{protocols =
                                                Core.Nothing,
                                              role' = Core.Nothing}

-- | This property is used to determine the nature of communication over this @SINGLE_MASTER@ signaling channel. If @WSS@ is specified, this API returns a websocket endpoint. If @HTTPS@ is specified, this API returns an @HTTPS@ endpoint.
--
-- /Note:/ Consider using 'protocols' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smcecProtocols :: Lens.Lens' SingleMasterChannelEndpointConfiguration (Core.Maybe (Core.NonEmpty Types.ChannelProtocol))
smcecProtocols = Lens.field @"protocols"
{-# INLINEABLE smcecProtocols #-}
{-# DEPRECATED protocols "Use generic-lens or generic-optics with 'protocols' instead"  #-}

-- | This property is used to determine messaging permissions in this @SINGLE_MASTER@ signaling channel. If @MASTER@ is specified, this API returns an endpoint that a client can use to receive offers from and send answers to any of the viewers on this signaling channel. If @VIEWER@ is specified, this API returns an endpoint that a client can use only to send offers to another @MASTER@ client on this signaling channel. 
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smcecRole :: Lens.Lens' SingleMasterChannelEndpointConfiguration (Core.Maybe Types.ChannelRole)
smcecRole = Lens.field @"role'"
{-# INLINEABLE smcecRole #-}
{-# DEPRECATED role' "Use generic-lens or generic-optics with 'role'' instead"  #-}

instance Core.FromJSON SingleMasterChannelEndpointConfiguration
         where
        toJSON SingleMasterChannelEndpointConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [("Protocols" Core..=) Core.<$> protocols,
                  ("Role" Core..=) Core.<$> role'])
