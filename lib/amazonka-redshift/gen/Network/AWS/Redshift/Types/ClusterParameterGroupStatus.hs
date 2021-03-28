{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ClusterParameterGroupStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.ClusterParameterGroupStatus
  ( ClusterParameterGroupStatus (..)
  -- * Smart constructor
  , mkClusterParameterGroupStatus
  -- * Lenses
  , cpgsClusterParameterStatusList
  , cpgsParameterApplyStatus
  , cpgsParameterGroupName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.ClusterParameterStatus as Types

-- | Describes the status of a parameter group.
--
-- /See:/ 'mkClusterParameterGroupStatus' smart constructor.
data ClusterParameterGroupStatus = ClusterParameterGroupStatus'
  { clusterParameterStatusList :: Core.Maybe [Types.ClusterParameterStatus]
    -- ^ The list of parameter statuses.
--
-- For more information about parameters and parameter groups, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups> in the /Amazon Redshift Cluster Management Guide/ .
  , parameterApplyStatus :: Core.Maybe Core.Text
    -- ^ The status of parameter updates.
  , parameterGroupName :: Core.Maybe Core.Text
    -- ^ The name of the cluster parameter group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClusterParameterGroupStatus' value with any optional fields omitted.
mkClusterParameterGroupStatus
    :: ClusterParameterGroupStatus
mkClusterParameterGroupStatus
  = ClusterParameterGroupStatus'{clusterParameterStatusList =
                                   Core.Nothing,
                                 parameterApplyStatus = Core.Nothing,
                                 parameterGroupName = Core.Nothing}

-- | The list of parameter statuses.
--
-- For more information about parameters and parameter groups, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups> in the /Amazon Redshift Cluster Management Guide/ .
--
-- /Note:/ Consider using 'clusterParameterStatusList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgsClusterParameterStatusList :: Lens.Lens' ClusterParameterGroupStatus (Core.Maybe [Types.ClusterParameterStatus])
cpgsClusterParameterStatusList = Lens.field @"clusterParameterStatusList"
{-# INLINEABLE cpgsClusterParameterStatusList #-}
{-# DEPRECATED clusterParameterStatusList "Use generic-lens or generic-optics with 'clusterParameterStatusList' instead"  #-}

-- | The status of parameter updates.
--
-- /Note:/ Consider using 'parameterApplyStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgsParameterApplyStatus :: Lens.Lens' ClusterParameterGroupStatus (Core.Maybe Core.Text)
cpgsParameterApplyStatus = Lens.field @"parameterApplyStatus"
{-# INLINEABLE cpgsParameterApplyStatus #-}
{-# DEPRECATED parameterApplyStatus "Use generic-lens or generic-optics with 'parameterApplyStatus' instead"  #-}

-- | The name of the cluster parameter group.
--
-- /Note:/ Consider using 'parameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgsParameterGroupName :: Lens.Lens' ClusterParameterGroupStatus (Core.Maybe Core.Text)
cpgsParameterGroupName = Lens.field @"parameterGroupName"
{-# INLINEABLE cpgsParameterGroupName #-}
{-# DEPRECATED parameterGroupName "Use generic-lens or generic-optics with 'parameterGroupName' instead"  #-}

instance Core.FromXML ClusterParameterGroupStatus where
        parseXML x
          = ClusterParameterGroupStatus' Core.<$>
              (x Core..@? "ClusterParameterStatusList" Core..<@>
                 Core.parseXMLList "member")
                Core.<*> x Core..@? "ParameterApplyStatus"
                Core.<*> x Core..@? "ParameterGroupName"
