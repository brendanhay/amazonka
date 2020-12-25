{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ClusterParameterGroupNameMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterParameterGroupNameMessage
  ( ClusterParameterGroupNameMessage (..),

    -- * Smart constructor
    mkClusterParameterGroupNameMessage,

    -- * Lenses
    cpgnmParameterGroupName,
    cpgnmParameterGroupStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.String as Types

-- |
--
-- /See:/ 'mkClusterParameterGroupNameMessage' smart constructor.
data ClusterParameterGroupNameMessage = ClusterParameterGroupNameMessage'
  { -- | The name of the cluster parameter group.
    parameterGroupName :: Core.Maybe Types.String,
    -- | The status of the parameter group. For example, if you made a change to a parameter group name-value pair, then the change could be pending a reboot of an associated cluster.
    parameterGroupStatus :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClusterParameterGroupNameMessage' value with any optional fields omitted.
mkClusterParameterGroupNameMessage ::
  ClusterParameterGroupNameMessage
mkClusterParameterGroupNameMessage =
  ClusterParameterGroupNameMessage'
    { parameterGroupName =
        Core.Nothing,
      parameterGroupStatus = Core.Nothing
    }

-- | The name of the cluster parameter group.
--
-- /Note:/ Consider using 'parameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgnmParameterGroupName :: Lens.Lens' ClusterParameterGroupNameMessage (Core.Maybe Types.String)
cpgnmParameterGroupName = Lens.field @"parameterGroupName"
{-# DEPRECATED cpgnmParameterGroupName "Use generic-lens or generic-optics with 'parameterGroupName' instead." #-}

-- | The status of the parameter group. For example, if you made a change to a parameter group name-value pair, then the change could be pending a reboot of an associated cluster.
--
-- /Note:/ Consider using 'parameterGroupStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgnmParameterGroupStatus :: Lens.Lens' ClusterParameterGroupNameMessage (Core.Maybe Types.String)
cpgnmParameterGroupStatus = Lens.field @"parameterGroupStatus"
{-# DEPRECATED cpgnmParameterGroupStatus "Use generic-lens or generic-optics with 'parameterGroupStatus' instead." #-}

instance Core.FromXML ClusterParameterGroupNameMessage where
  parseXML x =
    ClusterParameterGroupNameMessage'
      Core.<$> (x Core..@? "ParameterGroupName")
      Core.<*> (x Core..@? "ParameterGroupStatus")
