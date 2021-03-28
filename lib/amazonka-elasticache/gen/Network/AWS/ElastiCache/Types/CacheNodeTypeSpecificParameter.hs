{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.CacheNodeTypeSpecificParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.CacheNodeTypeSpecificParameter
  ( CacheNodeTypeSpecificParameter (..)
  -- * Smart constructor
  , mkCacheNodeTypeSpecificParameter
  -- * Lenses
  , cntspAllowedValues
  , cntspCacheNodeTypeSpecificValues
  , cntspChangeType
  , cntspDataType
  , cntspDescription
  , cntspIsModifiable
  , cntspMinimumEngineVersion
  , cntspParameterName
  , cntspSource
  ) where

import qualified Network.AWS.ElastiCache.Types.CacheNodeTypeSpecificValue as Types
import qualified Network.AWS.ElastiCache.Types.ChangeType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A parameter that has a different value for each cache node type it is applied to. For example, in a Redis cluster, a @cache.m1.large@ cache node type would have a larger @maxmemory@ value than a @cache.m1.small@ type.
--
-- /See:/ 'mkCacheNodeTypeSpecificParameter' smart constructor.
data CacheNodeTypeSpecificParameter = CacheNodeTypeSpecificParameter'
  { allowedValues :: Core.Maybe Core.Text
    -- ^ The valid range of values for the parameter.
  , cacheNodeTypeSpecificValues :: Core.Maybe [Types.CacheNodeTypeSpecificValue]
    -- ^ A list of cache node types and their corresponding values for this parameter.
  , changeType :: Core.Maybe Types.ChangeType
    -- ^ Indicates whether a change to the parameter is applied immediately or requires a reboot for the change to be applied. You can force a reboot or wait until the next maintenance window's reboot. For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.Rebooting.html Rebooting a Cluster> .
  , dataType :: Core.Maybe Core.Text
    -- ^ The valid data type for the parameter.
  , description :: Core.Maybe Core.Text
    -- ^ A description of the parameter.
  , isModifiable :: Core.Maybe Core.Bool
    -- ^ Indicates whether (@true@ ) or not (@false@ ) the parameter can be modified. Some parameters have security or operational implications that prevent them from being changed.
  , minimumEngineVersion :: Core.Maybe Core.Text
    -- ^ The earliest cache engine version to which the parameter can apply.
  , parameterName :: Core.Maybe Core.Text
    -- ^ The name of the parameter.
  , source :: Core.Maybe Core.Text
    -- ^ The source of the parameter value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CacheNodeTypeSpecificParameter' value with any optional fields omitted.
mkCacheNodeTypeSpecificParameter
    :: CacheNodeTypeSpecificParameter
mkCacheNodeTypeSpecificParameter
  = CacheNodeTypeSpecificParameter'{allowedValues = Core.Nothing,
                                    cacheNodeTypeSpecificValues = Core.Nothing,
                                    changeType = Core.Nothing, dataType = Core.Nothing,
                                    description = Core.Nothing, isModifiable = Core.Nothing,
                                    minimumEngineVersion = Core.Nothing,
                                    parameterName = Core.Nothing, source = Core.Nothing}

-- | The valid range of values for the parameter.
--
-- /Note:/ Consider using 'allowedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cntspAllowedValues :: Lens.Lens' CacheNodeTypeSpecificParameter (Core.Maybe Core.Text)
cntspAllowedValues = Lens.field @"allowedValues"
{-# INLINEABLE cntspAllowedValues #-}
{-# DEPRECATED allowedValues "Use generic-lens or generic-optics with 'allowedValues' instead"  #-}

-- | A list of cache node types and their corresponding values for this parameter.
--
-- /Note:/ Consider using 'cacheNodeTypeSpecificValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cntspCacheNodeTypeSpecificValues :: Lens.Lens' CacheNodeTypeSpecificParameter (Core.Maybe [Types.CacheNodeTypeSpecificValue])
cntspCacheNodeTypeSpecificValues = Lens.field @"cacheNodeTypeSpecificValues"
{-# INLINEABLE cntspCacheNodeTypeSpecificValues #-}
{-# DEPRECATED cacheNodeTypeSpecificValues "Use generic-lens or generic-optics with 'cacheNodeTypeSpecificValues' instead"  #-}

-- | Indicates whether a change to the parameter is applied immediately or requires a reboot for the change to be applied. You can force a reboot or wait until the next maintenance window's reboot. For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.Rebooting.html Rebooting a Cluster> .
--
-- /Note:/ Consider using 'changeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cntspChangeType :: Lens.Lens' CacheNodeTypeSpecificParameter (Core.Maybe Types.ChangeType)
cntspChangeType = Lens.field @"changeType"
{-# INLINEABLE cntspChangeType #-}
{-# DEPRECATED changeType "Use generic-lens or generic-optics with 'changeType' instead"  #-}

-- | The valid data type for the parameter.
--
-- /Note:/ Consider using 'dataType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cntspDataType :: Lens.Lens' CacheNodeTypeSpecificParameter (Core.Maybe Core.Text)
cntspDataType = Lens.field @"dataType"
{-# INLINEABLE cntspDataType #-}
{-# DEPRECATED dataType "Use generic-lens or generic-optics with 'dataType' instead"  #-}

-- | A description of the parameter.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cntspDescription :: Lens.Lens' CacheNodeTypeSpecificParameter (Core.Maybe Core.Text)
cntspDescription = Lens.field @"description"
{-# INLINEABLE cntspDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Indicates whether (@true@ ) or not (@false@ ) the parameter can be modified. Some parameters have security or operational implications that prevent them from being changed.
--
-- /Note:/ Consider using 'isModifiable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cntspIsModifiable :: Lens.Lens' CacheNodeTypeSpecificParameter (Core.Maybe Core.Bool)
cntspIsModifiable = Lens.field @"isModifiable"
{-# INLINEABLE cntspIsModifiable #-}
{-# DEPRECATED isModifiable "Use generic-lens or generic-optics with 'isModifiable' instead"  #-}

-- | The earliest cache engine version to which the parameter can apply.
--
-- /Note:/ Consider using 'minimumEngineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cntspMinimumEngineVersion :: Lens.Lens' CacheNodeTypeSpecificParameter (Core.Maybe Core.Text)
cntspMinimumEngineVersion = Lens.field @"minimumEngineVersion"
{-# INLINEABLE cntspMinimumEngineVersion #-}
{-# DEPRECATED minimumEngineVersion "Use generic-lens or generic-optics with 'minimumEngineVersion' instead"  #-}

-- | The name of the parameter.
--
-- /Note:/ Consider using 'parameterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cntspParameterName :: Lens.Lens' CacheNodeTypeSpecificParameter (Core.Maybe Core.Text)
cntspParameterName = Lens.field @"parameterName"
{-# INLINEABLE cntspParameterName #-}
{-# DEPRECATED parameterName "Use generic-lens or generic-optics with 'parameterName' instead"  #-}

-- | The source of the parameter value.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cntspSource :: Lens.Lens' CacheNodeTypeSpecificParameter (Core.Maybe Core.Text)
cntspSource = Lens.field @"source"
{-# INLINEABLE cntspSource #-}
{-# DEPRECATED source "Use generic-lens or generic-optics with 'source' instead"  #-}

instance Core.FromXML CacheNodeTypeSpecificParameter where
        parseXML x
          = CacheNodeTypeSpecificParameter' Core.<$>
              (x Core..@? "AllowedValues") Core.<*>
                x Core..@? "CacheNodeTypeSpecificValues" Core..<@>
                  Core.parseXMLList "CacheNodeTypeSpecificValue"
                Core.<*> x Core..@? "ChangeType"
                Core.<*> x Core..@? "DataType"
                Core.<*> x Core..@? "Description"
                Core.<*> x Core..@? "IsModifiable"
                Core.<*> x Core..@? "MinimumEngineVersion"
                Core.<*> x Core..@? "ParameterName"
                Core.<*> x Core..@? "Source"
