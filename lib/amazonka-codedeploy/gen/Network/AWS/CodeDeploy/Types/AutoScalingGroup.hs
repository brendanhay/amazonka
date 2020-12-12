{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.AutoScalingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.AutoScalingGroup
  ( AutoScalingGroup (..),

    -- * Smart constructor
    mkAutoScalingGroup,

    -- * Lenses
    asgHook,
    asgName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an Auto Scaling group.
--
-- /See:/ 'mkAutoScalingGroup' smart constructor.
data AutoScalingGroup = AutoScalingGroup'
  { hook ::
      Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AutoScalingGroup' with the minimum fields required to make a request.
--
-- * 'hook' - An Auto Scaling lifecycle event hook name.
-- * 'name' - The Auto Scaling group name.
mkAutoScalingGroup ::
  AutoScalingGroup
mkAutoScalingGroup =
  AutoScalingGroup' {hook = Lude.Nothing, name = Lude.Nothing}

-- | An Auto Scaling lifecycle event hook name.
--
-- /Note:/ Consider using 'hook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgHook :: Lens.Lens' AutoScalingGroup (Lude.Maybe Lude.Text)
asgHook = Lens.lens (hook :: AutoScalingGroup -> Lude.Maybe Lude.Text) (\s a -> s {hook = a} :: AutoScalingGroup)
{-# DEPRECATED asgHook "Use generic-lens or generic-optics with 'hook' instead." #-}

-- | The Auto Scaling group name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgName :: Lens.Lens' AutoScalingGroup (Lude.Maybe Lude.Text)
asgName = Lens.lens (name :: AutoScalingGroup -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: AutoScalingGroup)
{-# DEPRECATED asgName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON AutoScalingGroup where
  parseJSON =
    Lude.withObject
      "AutoScalingGroup"
      ( \x ->
          AutoScalingGroup'
            Lude.<$> (x Lude..:? "hook") Lude.<*> (x Lude..:? "name")
      )
