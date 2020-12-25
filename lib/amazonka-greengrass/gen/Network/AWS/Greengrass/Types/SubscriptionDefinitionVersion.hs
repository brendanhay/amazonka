{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.SubscriptionDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.SubscriptionDefinitionVersion
  ( SubscriptionDefinitionVersion (..),

    -- * Smart constructor
    mkSubscriptionDefinitionVersion,

    -- * Lenses
    sdvSubscriptions,
  )
where

import qualified Network.AWS.Greengrass.Types.Subscription as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a subscription definition version.
--
-- /See:/ 'mkSubscriptionDefinitionVersion' smart constructor.
newtype SubscriptionDefinitionVersion = SubscriptionDefinitionVersion'
  { -- | A list of subscriptions.
    subscriptions :: Core.Maybe [Types.Subscription]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SubscriptionDefinitionVersion' value with any optional fields omitted.
mkSubscriptionDefinitionVersion ::
  SubscriptionDefinitionVersion
mkSubscriptionDefinitionVersion =
  SubscriptionDefinitionVersion' {subscriptions = Core.Nothing}

-- | A list of subscriptions.
--
-- /Note:/ Consider using 'subscriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdvSubscriptions :: Lens.Lens' SubscriptionDefinitionVersion (Core.Maybe [Types.Subscription])
sdvSubscriptions = Lens.field @"subscriptions"
{-# DEPRECATED sdvSubscriptions "Use generic-lens or generic-optics with 'subscriptions' instead." #-}

instance Core.FromJSON SubscriptionDefinitionVersion where
  toJSON SubscriptionDefinitionVersion {..} =
    Core.object
      (Core.catMaybes [("Subscriptions" Core..=) Core.<$> subscriptions])

instance Core.FromJSON SubscriptionDefinitionVersion where
  parseJSON =
    Core.withObject "SubscriptionDefinitionVersion" Core.$
      \x ->
        SubscriptionDefinitionVersion'
          Core.<$> (x Core..:? "Subscriptions")
