{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.ELBInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.ELBInfo
  ( ELBInfo (..),

    -- * Smart constructor
    mkELBInfo,

    -- * Lenses
    elbiName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a load balancer in Elastic Load Balancing to use in a deployment. Instances are registered directly with a load balancer, and traffic is routed to the load balancer.
--
-- /See:/ 'mkELBInfo' smart constructor.
newtype ELBInfo = ELBInfo' {name :: Lude.Maybe Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ELBInfo' with the minimum fields required to make a request.
--
-- * 'name' - For blue/green deployments, the name of the load balancer that is used to route traffic from original instances to replacement instances in a blue/green deployment. For in-place deployments, the name of the load balancer that instances are deregistered from so they are not serving traffic during a deployment, and then re-registered with after the deployment is complete.
mkELBInfo ::
  ELBInfo
mkELBInfo = ELBInfo' {name = Lude.Nothing}

-- | For blue/green deployments, the name of the load balancer that is used to route traffic from original instances to replacement instances in a blue/green deployment. For in-place deployments, the name of the load balancer that instances are deregistered from so they are not serving traffic during a deployment, and then re-registered with after the deployment is complete.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elbiName :: Lens.Lens' ELBInfo (Lude.Maybe Lude.Text)
elbiName = Lens.lens (name :: ELBInfo -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ELBInfo)
{-# DEPRECATED elbiName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON ELBInfo where
  parseJSON =
    Lude.withObject
      "ELBInfo"
      (\x -> ELBInfo' Lude.<$> (x Lude..:? "name"))

instance Lude.ToJSON ELBInfo where
  toJSON ELBInfo' {..} =
    Lude.object (Lude.catMaybes [("name" Lude..=) Lude.<$> name])
