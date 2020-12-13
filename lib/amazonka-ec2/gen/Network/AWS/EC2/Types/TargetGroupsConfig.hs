{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TargetGroupsConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TargetGroupsConfig
  ( TargetGroupsConfig (..),

    -- * Smart constructor
    mkTargetGroupsConfig,

    -- * Lenses
    tgcTargetGroups,
  )
where

import Network.AWS.EC2.Types.TargetGroup
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the target groups to attach to a Spot Fleet. Spot Fleet registers the running Spot Instances with these target groups.
--
-- /See:/ 'mkTargetGroupsConfig' smart constructor.
newtype TargetGroupsConfig = TargetGroupsConfig'
  { -- | One or more target groups.
    targetGroups :: Lude.Maybe (Lude.NonEmpty TargetGroup)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TargetGroupsConfig' with the minimum fields required to make a request.
--
-- * 'targetGroups' - One or more target groups.
mkTargetGroupsConfig ::
  TargetGroupsConfig
mkTargetGroupsConfig =
  TargetGroupsConfig' {targetGroups = Lude.Nothing}

-- | One or more target groups.
--
-- /Note:/ Consider using 'targetGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgcTargetGroups :: Lens.Lens' TargetGroupsConfig (Lude.Maybe (Lude.NonEmpty TargetGroup))
tgcTargetGroups = Lens.lens (targetGroups :: TargetGroupsConfig -> Lude.Maybe (Lude.NonEmpty TargetGroup)) (\s a -> s {targetGroups = a} :: TargetGroupsConfig)
{-# DEPRECATED tgcTargetGroups "Use generic-lens or generic-optics with 'targetGroups' instead." #-}

instance Lude.FromXML TargetGroupsConfig where
  parseXML x =
    TargetGroupsConfig'
      Lude.<$> ( x Lude..@? "targetGroups" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLNonEmpty "item")
               )

instance Lude.ToQuery TargetGroupsConfig where
  toQuery TargetGroupsConfig' {..} =
    Lude.mconcat
      [ Lude.toQuery
          (Lude.toQueryList "TargetGroups" Lude.<$> targetGroups)
      ]
