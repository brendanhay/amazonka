{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TargetGroupInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TargetGroupInfo
  ( TargetGroupInfo (..),

    -- * Smart constructor
    mkTargetGroupInfo,

    -- * Lenses
    tgiName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a target group in Elastic Load Balancing to use in a deployment. Instances are registered as targets in a target group, and traffic is routed to the target group.
--
-- /See:/ 'mkTargetGroupInfo' smart constructor.
newtype TargetGroupInfo = TargetGroupInfo'
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

-- | Creates a value of 'TargetGroupInfo' with the minimum fields required to make a request.
--
-- * 'name' - For blue/green deployments, the name of the target group that instances in the original environment are deregistered from, and instances in the replacement environment are registered with. For in-place deployments, the name of the target group that instances are deregistered from, so they are not serving traffic during a deployment, and then re-registered with after the deployment is complete.
mkTargetGroupInfo ::
  TargetGroupInfo
mkTargetGroupInfo = TargetGroupInfo' {name = Lude.Nothing}

-- | For blue/green deployments, the name of the target group that instances in the original environment are deregistered from, and instances in the replacement environment are registered with. For in-place deployments, the name of the target group that instances are deregistered from, so they are not serving traffic during a deployment, and then re-registered with after the deployment is complete.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgiName :: Lens.Lens' TargetGroupInfo (Lude.Maybe Lude.Text)
tgiName = Lens.lens (name :: TargetGroupInfo -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: TargetGroupInfo)
{-# DEPRECATED tgiName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON TargetGroupInfo where
  parseJSON =
    Lude.withObject
      "TargetGroupInfo"
      (\x -> TargetGroupInfo' Lude.<$> (x Lude..:? "name"))

instance Lude.ToJSON TargetGroupInfo where
  toJSON TargetGroupInfo' {..} =
    Lude.object (Lude.catMaybes [("name" Lude..=) Lude.<$> name])
