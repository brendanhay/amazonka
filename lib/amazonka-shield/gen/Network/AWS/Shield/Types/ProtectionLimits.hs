-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.ProtectionLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.ProtectionLimits
  ( ProtectionLimits (..),

    -- * Smart constructor
    mkProtectionLimits,

    -- * Lenses
    plProtectedResourceTypeLimits,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Shield.Types.Limit

-- | Limits settings on protections for your subscription.
--
-- /See:/ 'mkProtectionLimits' smart constructor.
newtype ProtectionLimits = ProtectionLimits'
  { protectedResourceTypeLimits ::
      [Limit]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProtectionLimits' with the minimum fields required to make a request.
--
-- * 'protectedResourceTypeLimits' - The maximum number of resource types that you can specify in a protection.
mkProtectionLimits ::
  ProtectionLimits
mkProtectionLimits =
  ProtectionLimits' {protectedResourceTypeLimits = Lude.mempty}

-- | The maximum number of resource types that you can specify in a protection.
--
-- /Note:/ Consider using 'protectedResourceTypeLimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plProtectedResourceTypeLimits :: Lens.Lens' ProtectionLimits [Limit]
plProtectedResourceTypeLimits = Lens.lens (protectedResourceTypeLimits :: ProtectionLimits -> [Limit]) (\s a -> s {protectedResourceTypeLimits = a} :: ProtectionLimits)
{-# DEPRECATED plProtectedResourceTypeLimits "Use generic-lens or generic-optics with 'protectedResourceTypeLimits' instead." #-}

instance Lude.FromJSON ProtectionLimits where
  parseJSON =
    Lude.withObject
      "ProtectionLimits"
      ( \x ->
          ProtectionLimits'
            Lude.<$> (x Lude..:? "ProtectedResourceTypeLimits" Lude..!= Lude.mempty)
      )
