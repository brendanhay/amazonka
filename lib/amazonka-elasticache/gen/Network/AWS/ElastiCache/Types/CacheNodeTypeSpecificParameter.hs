{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.CacheNodeTypeSpecificParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.CacheNodeTypeSpecificParameter
  ( CacheNodeTypeSpecificParameter (..),

    -- * Smart constructor
    mkCacheNodeTypeSpecificParameter,

    -- * Lenses
    cntspAllowedValues,
    cntspCacheNodeTypeSpecificValues,
    cntspChangeType,
    cntspDataType,
    cntspDescription,
    cntspIsModifiable,
    cntspMinimumEngineVersion,
    cntspParameterName,
    cntspSource,
  )
where

import qualified Network.AWS.ElastiCache.Types.CacheNodeTypeSpecificValue as Types
import qualified Network.AWS.ElastiCache.Types.ChangeType as Types
import qualified Network.AWS.ElastiCache.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A parameter that has a different value for each cache node type it is applied to. For example, in a Redis cluster, a @cache.m1.large@ cache node type would have a larger @maxmemory@ value than a @cache.m1.small@ type.
--
-- /See:/ 'mkCacheNodeTypeSpecificParameter' smart constructor.
data CacheNodeTypeSpecificParameter = CacheNodeTypeSpecificParameter'
  { -- | The valid range of values for the parameter.
    allowedValues :: Core.Maybe Types.String,
    -- | A list of cache node types and their corresponding values for this parameter.
    cacheNodeTypeSpecificValues :: Core.Maybe [Types.CacheNodeTypeSpecificValue],
    -- | Indicates whether a change to the parameter is applied immediately or requires a reboot for the change to be applied. You can force a reboot or wait until the next maintenance window's reboot. For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.Rebooting.html Rebooting a Cluster> .
    changeType :: Core.Maybe Types.ChangeType,
    -- | The valid data type for the parameter.
    dataType :: Core.Maybe Types.String,
    -- | A description of the parameter.
    description :: Core.Maybe Types.String,
    -- | Indicates whether (@true@ ) or not (@false@ ) the parameter can be modified. Some parameters have security or operational implications that prevent them from being changed.
    isModifiable :: Core.Maybe Core.Bool,
    -- | The earliest cache engine version to which the parameter can apply.
    minimumEngineVersion :: Core.Maybe Types.String,
    -- | The name of the parameter.
    parameterName :: Core.Maybe Types.String,
    -- | The source of the parameter value.
    source :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CacheNodeTypeSpecificParameter' value with any optional fields omitted.
mkCacheNodeTypeSpecificParameter ::
  CacheNodeTypeSpecificParameter
mkCacheNodeTypeSpecificParameter =
  CacheNodeTypeSpecificParameter'
    { allowedValues = Core.Nothing,
      cacheNodeTypeSpecificValues = Core.Nothing,
      changeType = Core.Nothing,
      dataType = Core.Nothing,
      description = Core.Nothing,
      isModifiable = Core.Nothing,
      minimumEngineVersion = Core.Nothing,
      parameterName = Core.Nothing,
      source = Core.Nothing
    }

-- | The valid range of values for the parameter.
--
-- /Note:/ Consider using 'allowedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cntspAllowedValues :: Lens.Lens' CacheNodeTypeSpecificParameter (Core.Maybe Types.String)
cntspAllowedValues = Lens.field @"allowedValues"
{-# DEPRECATED cntspAllowedValues "Use generic-lens or generic-optics with 'allowedValues' instead." #-}

-- | A list of cache node types and their corresponding values for this parameter.
--
-- /Note:/ Consider using 'cacheNodeTypeSpecificValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cntspCacheNodeTypeSpecificValues :: Lens.Lens' CacheNodeTypeSpecificParameter (Core.Maybe [Types.CacheNodeTypeSpecificValue])
cntspCacheNodeTypeSpecificValues = Lens.field @"cacheNodeTypeSpecificValues"
{-# DEPRECATED cntspCacheNodeTypeSpecificValues "Use generic-lens or generic-optics with 'cacheNodeTypeSpecificValues' instead." #-}

-- | Indicates whether a change to the parameter is applied immediately or requires a reboot for the change to be applied. You can force a reboot or wait until the next maintenance window's reboot. For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.Rebooting.html Rebooting a Cluster> .
--
-- /Note:/ Consider using 'changeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cntspChangeType :: Lens.Lens' CacheNodeTypeSpecificParameter (Core.Maybe Types.ChangeType)
cntspChangeType = Lens.field @"changeType"
{-# DEPRECATED cntspChangeType "Use generic-lens or generic-optics with 'changeType' instead." #-}

-- | The valid data type for the parameter.
--
-- /Note:/ Consider using 'dataType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cntspDataType :: Lens.Lens' CacheNodeTypeSpecificParameter (Core.Maybe Types.String)
cntspDataType = Lens.field @"dataType"
{-# DEPRECATED cntspDataType "Use generic-lens or generic-optics with 'dataType' instead." #-}

-- | A description of the parameter.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cntspDescription :: Lens.Lens' CacheNodeTypeSpecificParameter (Core.Maybe Types.String)
cntspDescription = Lens.field @"description"
{-# DEPRECATED cntspDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Indicates whether (@true@ ) or not (@false@ ) the parameter can be modified. Some parameters have security or operational implications that prevent them from being changed.
--
-- /Note:/ Consider using 'isModifiable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cntspIsModifiable :: Lens.Lens' CacheNodeTypeSpecificParameter (Core.Maybe Core.Bool)
cntspIsModifiable = Lens.field @"isModifiable"
{-# DEPRECATED cntspIsModifiable "Use generic-lens or generic-optics with 'isModifiable' instead." #-}

-- | The earliest cache engine version to which the parameter can apply.
--
-- /Note:/ Consider using 'minimumEngineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cntspMinimumEngineVersion :: Lens.Lens' CacheNodeTypeSpecificParameter (Core.Maybe Types.String)
cntspMinimumEngineVersion = Lens.field @"minimumEngineVersion"
{-# DEPRECATED cntspMinimumEngineVersion "Use generic-lens or generic-optics with 'minimumEngineVersion' instead." #-}

-- | The name of the parameter.
--
-- /Note:/ Consider using 'parameterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cntspParameterName :: Lens.Lens' CacheNodeTypeSpecificParameter (Core.Maybe Types.String)
cntspParameterName = Lens.field @"parameterName"
{-# DEPRECATED cntspParameterName "Use generic-lens or generic-optics with 'parameterName' instead." #-}

-- | The source of the parameter value.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cntspSource :: Lens.Lens' CacheNodeTypeSpecificParameter (Core.Maybe Types.String)
cntspSource = Lens.field @"source"
{-# DEPRECATED cntspSource "Use generic-lens or generic-optics with 'source' instead." #-}

instance Core.FromXML CacheNodeTypeSpecificParameter where
  parseXML x =
    CacheNodeTypeSpecificParameter'
      Core.<$> (x Core..@? "AllowedValues")
      Core.<*> ( x Core..@? "CacheNodeTypeSpecificValues"
                   Core..<@> Core.parseXMLList "CacheNodeTypeSpecificValue"
               )
      Core.<*> (x Core..@? "ChangeType")
      Core.<*> (x Core..@? "DataType")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "IsModifiable")
      Core.<*> (x Core..@? "MinimumEngineVersion")
      Core.<*> (x Core..@? "ParameterName")
      Core.<*> (x Core..@? "Source")
