-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.LifecyclePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.LifecyclePolicy
  ( LifecyclePolicy (..),

    -- * Smart constructor
    mkLifecyclePolicy,

    -- * Lenses
    lpTransitionToIA,
  )
where

import Network.AWS.EFS.Types.TransitionToIARules
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a policy used by EFS lifecycle management to transition files to the Infrequent Access (IA) storage class.
--
-- /See:/ 'mkLifecyclePolicy' smart constructor.
newtype LifecyclePolicy = LifecyclePolicy'
  { transitionToIA ::
      Lude.Maybe TransitionToIARules
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LifecyclePolicy' with the minimum fields required to make a request.
--
-- * 'transitionToIA' - A value that describes the period of time that a file is not accessed, after which it transitions to the IA storage class. Metadata operations such as listing the contents of a directory don't count as file access events.
mkLifecyclePolicy ::
  LifecyclePolicy
mkLifecyclePolicy = LifecyclePolicy' {transitionToIA = Lude.Nothing}

-- | A value that describes the period of time that a file is not accessed, after which it transitions to the IA storage class. Metadata operations such as listing the contents of a directory don't count as file access events.
--
-- /Note:/ Consider using 'transitionToIA' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpTransitionToIA :: Lens.Lens' LifecyclePolicy (Lude.Maybe TransitionToIARules)
lpTransitionToIA = Lens.lens (transitionToIA :: LifecyclePolicy -> Lude.Maybe TransitionToIARules) (\s a -> s {transitionToIA = a} :: LifecyclePolicy)
{-# DEPRECATED lpTransitionToIA "Use generic-lens or generic-optics with 'transitionToIA' instead." #-}

instance Lude.FromJSON LifecyclePolicy where
  parseJSON =
    Lude.withObject
      "LifecyclePolicy"
      (\x -> LifecyclePolicy' Lude.<$> (x Lude..:? "TransitionToIA"))

instance Lude.ToJSON LifecyclePolicy where
  toJSON LifecyclePolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [("TransitionToIA" Lude..=) Lude.<$> transitionToIA]
      )
