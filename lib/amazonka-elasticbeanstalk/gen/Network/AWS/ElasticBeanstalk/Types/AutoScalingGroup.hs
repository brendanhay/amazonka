-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.AutoScalingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.AutoScalingGroup
  ( AutoScalingGroup (..),

    -- * Smart constructor
    mkAutoScalingGroup,

    -- * Lenses
    asgName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an Auto Scaling launch configuration.
--
-- /See:/ 'mkAutoScalingGroup' smart constructor.
newtype AutoScalingGroup = AutoScalingGroup'
  { name ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AutoScalingGroup' with the minimum fields required to make a request.
--
-- * 'name' - The name of the @AutoScalingGroup@ .
mkAutoScalingGroup ::
  AutoScalingGroup
mkAutoScalingGroup = AutoScalingGroup' {name = Lude.Nothing}

-- | The name of the @AutoScalingGroup@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgName :: Lens.Lens' AutoScalingGroup (Lude.Maybe Lude.Text)
asgName = Lens.lens (name :: AutoScalingGroup -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: AutoScalingGroup)
{-# DEPRECATED asgName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromXML AutoScalingGroup where
  parseXML x = AutoScalingGroup' Lude.<$> (x Lude..@? "Name")
