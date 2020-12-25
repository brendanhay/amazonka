{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexMediaConnectOutputDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexMediaConnectOutputDestinationSettings
  ( MultiplexMediaConnectOutputDestinationSettings (..),

    -- * Smart constructor
    mkMultiplexMediaConnectOutputDestinationSettings,

    -- * Lenses
    mmcodsEntitlementArn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Multiplex MediaConnect output destination settings.
--
-- /See:/ 'mkMultiplexMediaConnectOutputDestinationSettings' smart constructor.
newtype MultiplexMediaConnectOutputDestinationSettings = MultiplexMediaConnectOutputDestinationSettings'
  { -- | The MediaConnect entitlement ARN available as a Flow source.
    entitlementArn :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'MultiplexMediaConnectOutputDestinationSettings' value with any optional fields omitted.
mkMultiplexMediaConnectOutputDestinationSettings ::
  MultiplexMediaConnectOutputDestinationSettings
mkMultiplexMediaConnectOutputDestinationSettings =
  MultiplexMediaConnectOutputDestinationSettings'
    { entitlementArn =
        Core.Nothing
    }

-- | The MediaConnect entitlement ARN available as a Flow source.
--
-- /Note:/ Consider using 'entitlementArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mmcodsEntitlementArn :: Lens.Lens' MultiplexMediaConnectOutputDestinationSettings (Core.Maybe Core.Text)
mmcodsEntitlementArn = Lens.field @"entitlementArn"
{-# DEPRECATED mmcodsEntitlementArn "Use generic-lens or generic-optics with 'entitlementArn' instead." #-}

instance
  Core.FromJSON
    MultiplexMediaConnectOutputDestinationSettings
  where
  parseJSON =
    Core.withObject "MultiplexMediaConnectOutputDestinationSettings" Core.$
      \x ->
        MultiplexMediaConnectOutputDestinationSettings'
          Core.<$> (x Core..:? "entitlementArn")
