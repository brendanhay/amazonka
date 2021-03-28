{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TargetGroupsConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TargetGroupsConfig
  ( TargetGroupsConfig (..)
  -- * Smart constructor
  , mkTargetGroupsConfig
  -- * Lenses
  , tgcTargetGroups
  ) where

import qualified Network.AWS.EC2.Types.TargetGroup as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the target groups to attach to a Spot Fleet. Spot Fleet registers the running Spot Instances with these target groups.
--
-- /See:/ 'mkTargetGroupsConfig' smart constructor.
newtype TargetGroupsConfig = TargetGroupsConfig'
  { targetGroups :: Core.Maybe (Core.NonEmpty Types.TargetGroup)
    -- ^ One or more target groups.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TargetGroupsConfig' value with any optional fields omitted.
mkTargetGroupsConfig
    :: TargetGroupsConfig
mkTargetGroupsConfig
  = TargetGroupsConfig'{targetGroups = Core.Nothing}

-- | One or more target groups.
--
-- /Note:/ Consider using 'targetGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgcTargetGroups :: Lens.Lens' TargetGroupsConfig (Core.Maybe (Core.NonEmpty Types.TargetGroup))
tgcTargetGroups = Lens.field @"targetGroups"
{-# INLINEABLE tgcTargetGroups #-}
{-# DEPRECATED targetGroups "Use generic-lens or generic-optics with 'targetGroups' instead"  #-}

instance Core.ToQuery TargetGroupsConfig where
        toQuery TargetGroupsConfig{..}
          = Core.maybe Core.mempty (Core.toQueryList "TargetGroups")
              targetGroups

instance Core.FromXML TargetGroupsConfig where
        parseXML x
          = TargetGroupsConfig' Core.<$>
              (x Core..@? "targetGroups" Core..<@> Core.parseXMLNonEmpty "item")
