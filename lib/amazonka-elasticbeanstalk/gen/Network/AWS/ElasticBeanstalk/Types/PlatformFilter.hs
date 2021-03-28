{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.PlatformFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.PlatformFilter
  ( PlatformFilter (..)
  -- * Smart constructor
  , mkPlatformFilter
  -- * Lenses
  , pfOperator
  , pfType
  , pfValues
  ) where

import qualified Network.AWS.ElasticBeanstalk.Types.Operator as Types
import qualified Network.AWS.ElasticBeanstalk.Types.PlatformFilterType as Types
import qualified Network.AWS.ElasticBeanstalk.Types.PlatformFilterValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes criteria to restrict the results when listing platform versions.
--
-- The filter is evaluated as follows: @Type Operator Values[1]@ 
--
-- /See:/ 'mkPlatformFilter' smart constructor.
data PlatformFilter = PlatformFilter'
  { operator :: Core.Maybe Types.Operator
    -- ^ The operator to apply to the @Type@ with each of the @Values@ .
--
-- Valid values: @=@ | @!=@ | @<@ | @<=@ | @>@ | @>=@ | @contains@ | @begins_with@ | @ends_with@ 
  , type' :: Core.Maybe Types.PlatformFilterType
    -- ^ The platform version attribute to which the filter values are applied.
--
-- Valid values: @PlatformName@ | @PlatformVersion@ | @PlatformStatus@ | @PlatformBranchName@ | @PlatformLifecycleState@ | @PlatformOwner@ | @SupportedTier@ | @SupportedAddon@ | @ProgrammingLanguageName@ | @OperatingSystemName@ 
  , values :: Core.Maybe [Types.PlatformFilterValue]
    -- ^ The list of values applied to the filtering platform version attribute. Only one value is supported for all current operators.
--
-- The following list shows valid filter values for some filter attributes.
--
--     * @PlatformStatus@ : @Creating@ | @Failed@ | @Ready@ | @Deleting@ | @Deleted@ 
--
--
--     * @PlatformLifecycleState@ : @recommended@ 
--
--
--     * @SupportedTier@ : @WebServer/Standard@ | @Worker/SQS/HTTP@ 
--
--
--     * @SupportedAddon@ : @Log/S3@ | @Monitoring/Healthd@ | @WorkerDaemon/SQSD@ 
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PlatformFilter' value with any optional fields omitted.
mkPlatformFilter
    :: PlatformFilter
mkPlatformFilter
  = PlatformFilter'{operator = Core.Nothing, type' = Core.Nothing,
                    values = Core.Nothing}

-- | The operator to apply to the @Type@ with each of the @Values@ .
--
-- Valid values: @=@ | @!=@ | @<@ | @<=@ | @>@ | @>=@ | @contains@ | @begins_with@ | @ends_with@ 
--
-- /Note:/ Consider using 'operator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfOperator :: Lens.Lens' PlatformFilter (Core.Maybe Types.Operator)
pfOperator = Lens.field @"operator"
{-# INLINEABLE pfOperator #-}
{-# DEPRECATED operator "Use generic-lens or generic-optics with 'operator' instead"  #-}

-- | The platform version attribute to which the filter values are applied.
--
-- Valid values: @PlatformName@ | @PlatformVersion@ | @PlatformStatus@ | @PlatformBranchName@ | @PlatformLifecycleState@ | @PlatformOwner@ | @SupportedTier@ | @SupportedAddon@ | @ProgrammingLanguageName@ | @OperatingSystemName@ 
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfType :: Lens.Lens' PlatformFilter (Core.Maybe Types.PlatformFilterType)
pfType = Lens.field @"type'"
{-# INLINEABLE pfType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The list of values applied to the filtering platform version attribute. Only one value is supported for all current operators.
--
-- The following list shows valid filter values for some filter attributes.
--
--     * @PlatformStatus@ : @Creating@ | @Failed@ | @Ready@ | @Deleting@ | @Deleted@ 
--
--
--     * @PlatformLifecycleState@ : @recommended@ 
--
--
--     * @SupportedTier@ : @WebServer/Standard@ | @Worker/SQS/HTTP@ 
--
--
--     * @SupportedAddon@ : @Log/S3@ | @Monitoring/Healthd@ | @WorkerDaemon/SQSD@ 
--
--
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfValues :: Lens.Lens' PlatformFilter (Core.Maybe [Types.PlatformFilterValue])
pfValues = Lens.field @"values"
{-# INLINEABLE pfValues #-}
{-# DEPRECATED values "Use generic-lens or generic-optics with 'values' instead"  #-}

instance Core.ToQuery PlatformFilter where
        toQuery PlatformFilter{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Operator") operator
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Type") type'
              Core.<>
              Core.toQueryPair "Values"
                (Core.maybe Core.mempty (Core.toQueryList "member") values)
