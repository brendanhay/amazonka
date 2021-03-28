{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ExecutionEngineConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.ExecutionEngineConfig
  ( ExecutionEngineConfig (..)
  -- * Smart constructor
  , mkExecutionEngineConfig
  -- * Lenses
  , eecId
  , eecMasterInstanceSecurityGroupId
  , eecType
  ) where

import qualified Network.AWS.EMR.Types.ExecutionEngineType as Types
import qualified Network.AWS.EMR.Types.XmlStringMaxLen256 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the execution engine (cluster) to run the notebook and perform the notebook execution, for example, an EMR cluster.
--
-- /See:/ 'mkExecutionEngineConfig' smart constructor.
data ExecutionEngineConfig = ExecutionEngineConfig'
  { id :: Types.XmlStringMaxLen256
    -- ^ The unique identifier of the execution engine. For an EMR cluster, this is the cluster ID.
  , masterInstanceSecurityGroupId :: Core.Maybe Types.XmlStringMaxLen256
    -- ^ An optional unique ID of an EC2 security group to associate with the master instance of the EMR cluster for this notebook execution. For more information see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-managed-notebooks-security-groups.html Specifying EC2 Security Groups for EMR Notebooks> in the /EMR Management Guide/ .
  , type' :: Core.Maybe Types.ExecutionEngineType
    -- ^ The type of execution engine. A value of @EMR@ specifies an EMR cluster.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExecutionEngineConfig' value with any optional fields omitted.
mkExecutionEngineConfig
    :: Types.XmlStringMaxLen256 -- ^ 'id'
    -> ExecutionEngineConfig
mkExecutionEngineConfig id
  = ExecutionEngineConfig'{id,
                           masterInstanceSecurityGroupId = Core.Nothing, type' = Core.Nothing}

-- | The unique identifier of the execution engine. For an EMR cluster, this is the cluster ID.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eecId :: Lens.Lens' ExecutionEngineConfig Types.XmlStringMaxLen256
eecId = Lens.field @"id"
{-# INLINEABLE eecId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | An optional unique ID of an EC2 security group to associate with the master instance of the EMR cluster for this notebook execution. For more information see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-managed-notebooks-security-groups.html Specifying EC2 Security Groups for EMR Notebooks> in the /EMR Management Guide/ .
--
-- /Note:/ Consider using 'masterInstanceSecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eecMasterInstanceSecurityGroupId :: Lens.Lens' ExecutionEngineConfig (Core.Maybe Types.XmlStringMaxLen256)
eecMasterInstanceSecurityGroupId = Lens.field @"masterInstanceSecurityGroupId"
{-# INLINEABLE eecMasterInstanceSecurityGroupId #-}
{-# DEPRECATED masterInstanceSecurityGroupId "Use generic-lens or generic-optics with 'masterInstanceSecurityGroupId' instead"  #-}

-- | The type of execution engine. A value of @EMR@ specifies an EMR cluster.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eecType :: Lens.Lens' ExecutionEngineConfig (Core.Maybe Types.ExecutionEngineType)
eecType = Lens.field @"type'"
{-# INLINEABLE eecType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON ExecutionEngineConfig where
        toJSON ExecutionEngineConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Id" Core..= id),
                  ("MasterInstanceSecurityGroupId" Core..=) Core.<$>
                    masterInstanceSecurityGroupId,
                  ("Type" Core..=) Core.<$> type'])

instance Core.FromJSON ExecutionEngineConfig where
        parseJSON
          = Core.withObject "ExecutionEngineConfig" Core.$
              \ x ->
                ExecutionEngineConfig' Core.<$>
                  (x Core..: "Id") Core.<*>
                    x Core..:? "MasterInstanceSecurityGroupId"
                    Core.<*> x Core..:? "Type"
