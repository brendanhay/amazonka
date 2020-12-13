{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.SubscriptionLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.SubscriptionLimits
  ( SubscriptionLimits (..),

    -- * Smart constructor
    mkSubscriptionLimits,

    -- * Lenses
    slProtectionGroupLimits,
    slProtectionLimits,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Shield.Types.ProtectionGroupLimits
import Network.AWS.Shield.Types.ProtectionLimits

-- | Limits settings for your subscription.
--
-- /See:/ 'mkSubscriptionLimits' smart constructor.
data SubscriptionLimits = SubscriptionLimits'
  { -- | Limits settings on protection groups for your subscription.
    protectionGroupLimits :: ProtectionGroupLimits,
    -- | Limits settings on protections for your subscription.
    protectionLimits :: ProtectionLimits
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SubscriptionLimits' with the minimum fields required to make a request.
--
-- * 'protectionGroupLimits' - Limits settings on protection groups for your subscription.
-- * 'protectionLimits' - Limits settings on protections for your subscription.
mkSubscriptionLimits ::
  -- | 'protectionGroupLimits'
  ProtectionGroupLimits ->
  -- | 'protectionLimits'
  ProtectionLimits ->
  SubscriptionLimits
mkSubscriptionLimits pProtectionGroupLimits_ pProtectionLimits_ =
  SubscriptionLimits'
    { protectionGroupLimits =
        pProtectionGroupLimits_,
      protectionLimits = pProtectionLimits_
    }

-- | Limits settings on protection groups for your subscription.
--
-- /Note:/ Consider using 'protectionGroupLimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slProtectionGroupLimits :: Lens.Lens' SubscriptionLimits ProtectionGroupLimits
slProtectionGroupLimits = Lens.lens (protectionGroupLimits :: SubscriptionLimits -> ProtectionGroupLimits) (\s a -> s {protectionGroupLimits = a} :: SubscriptionLimits)
{-# DEPRECATED slProtectionGroupLimits "Use generic-lens or generic-optics with 'protectionGroupLimits' instead." #-}

-- | Limits settings on protections for your subscription.
--
-- /Note:/ Consider using 'protectionLimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slProtectionLimits :: Lens.Lens' SubscriptionLimits ProtectionLimits
slProtectionLimits = Lens.lens (protectionLimits :: SubscriptionLimits -> ProtectionLimits) (\s a -> s {protectionLimits = a} :: SubscriptionLimits)
{-# DEPRECATED slProtectionLimits "Use generic-lens or generic-optics with 'protectionLimits' instead." #-}

instance Lude.FromJSON SubscriptionLimits where
  parseJSON =
    Lude.withObject
      "SubscriptionLimits"
      ( \x ->
          SubscriptionLimits'
            Lude.<$> (x Lude..: "ProtectionGroupLimits")
            Lude.<*> (x Lude..: "ProtectionLimits")
      )
