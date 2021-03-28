{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ClusterVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.ClusterVersion
  ( ClusterVersion (..)
  -- * Smart constructor
  , mkClusterVersion
  -- * Lenses
  , cvClusterParameterGroupFamily
  , cvClusterVersion
  , cvDescription
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types

-- | Describes a cluster version, including the parameter group family and description of the version.
--
-- /See:/ 'mkClusterVersion' smart constructor.
data ClusterVersion = ClusterVersion'
  { clusterParameterGroupFamily :: Core.Maybe Core.Text
    -- ^ The name of the cluster parameter group family for the cluster.
  , clusterVersion :: Core.Maybe Core.Text
    -- ^ The version number used by the cluster.
  , description :: Core.Maybe Core.Text
    -- ^ The description of the cluster version.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClusterVersion' value with any optional fields omitted.
mkClusterVersion
    :: ClusterVersion
mkClusterVersion
  = ClusterVersion'{clusterParameterGroupFamily = Core.Nothing,
                    clusterVersion = Core.Nothing, description = Core.Nothing}

-- | The name of the cluster parameter group family for the cluster.
--
-- /Note:/ Consider using 'clusterParameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvClusterParameterGroupFamily :: Lens.Lens' ClusterVersion (Core.Maybe Core.Text)
cvClusterParameterGroupFamily = Lens.field @"clusterParameterGroupFamily"
{-# INLINEABLE cvClusterParameterGroupFamily #-}
{-# DEPRECATED clusterParameterGroupFamily "Use generic-lens or generic-optics with 'clusterParameterGroupFamily' instead"  #-}

-- | The version number used by the cluster.
--
-- /Note:/ Consider using 'clusterVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvClusterVersion :: Lens.Lens' ClusterVersion (Core.Maybe Core.Text)
cvClusterVersion = Lens.field @"clusterVersion"
{-# INLINEABLE cvClusterVersion #-}
{-# DEPRECATED clusterVersion "Use generic-lens or generic-optics with 'clusterVersion' instead"  #-}

-- | The description of the cluster version.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvDescription :: Lens.Lens' ClusterVersion (Core.Maybe Core.Text)
cvDescription = Lens.field @"description"
{-# INLINEABLE cvDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

instance Core.FromXML ClusterVersion where
        parseXML x
          = ClusterVersion' Core.<$>
              (x Core..@? "ClusterParameterGroupFamily") Core.<*>
                x Core..@? "ClusterVersion"
                Core.<*> x Core..@? "Description"
