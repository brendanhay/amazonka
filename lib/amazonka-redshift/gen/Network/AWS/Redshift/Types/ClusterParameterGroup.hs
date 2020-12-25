{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ClusterParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterParameterGroup
  ( ClusterParameterGroup (..),

    -- * Smart constructor
    mkClusterParameterGroup,

    -- * Lenses
    cpgDescription,
    cpgParameterGroupFamily,
    cpgParameterGroupName,
    cpgTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.Description as Types
import qualified Network.AWS.Redshift.Types.ParameterGroupFamily as Types
import qualified Network.AWS.Redshift.Types.ParameterGroupName as Types
import qualified Network.AWS.Redshift.Types.Tag as Types

-- | Describes a parameter group.
--
-- /See:/ 'mkClusterParameterGroup' smart constructor.
data ClusterParameterGroup = ClusterParameterGroup'
  { -- | The description of the parameter group.
    description :: Core.Maybe Types.Description,
    -- | The name of the cluster parameter group family that this cluster parameter group is compatible with.
    parameterGroupFamily :: Core.Maybe Types.ParameterGroupFamily,
    -- | The name of the cluster parameter group.
    parameterGroupName :: Core.Maybe Types.ParameterGroupName,
    -- | The list of tags for the cluster parameter group.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClusterParameterGroup' value with any optional fields omitted.
mkClusterParameterGroup ::
  ClusterParameterGroup
mkClusterParameterGroup =
  ClusterParameterGroup'
    { description = Core.Nothing,
      parameterGroupFamily = Core.Nothing,
      parameterGroupName = Core.Nothing,
      tags = Core.Nothing
    }

-- | The description of the parameter group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgDescription :: Lens.Lens' ClusterParameterGroup (Core.Maybe Types.Description)
cpgDescription = Lens.field @"description"
{-# DEPRECATED cpgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the cluster parameter group family that this cluster parameter group is compatible with.
--
-- /Note:/ Consider using 'parameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgParameterGroupFamily :: Lens.Lens' ClusterParameterGroup (Core.Maybe Types.ParameterGroupFamily)
cpgParameterGroupFamily = Lens.field @"parameterGroupFamily"
{-# DEPRECATED cpgParameterGroupFamily "Use generic-lens or generic-optics with 'parameterGroupFamily' instead." #-}

-- | The name of the cluster parameter group.
--
-- /Note:/ Consider using 'parameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgParameterGroupName :: Lens.Lens' ClusterParameterGroup (Core.Maybe Types.ParameterGroupName)
cpgParameterGroupName = Lens.field @"parameterGroupName"
{-# DEPRECATED cpgParameterGroupName "Use generic-lens or generic-optics with 'parameterGroupName' instead." #-}

-- | The list of tags for the cluster parameter group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgTags :: Lens.Lens' ClusterParameterGroup (Core.Maybe [Types.Tag])
cpgTags = Lens.field @"tags"
{-# DEPRECATED cpgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromXML ClusterParameterGroup where
  parseXML x =
    ClusterParameterGroup'
      Core.<$> (x Core..@? "Description")
      Core.<*> (x Core..@? "ParameterGroupFamily")
      Core.<*> (x Core..@? "ParameterGroupName")
      Core.<*> (x Core..@? "Tags" Core..<@> Core.parseXMLList "Tag")
