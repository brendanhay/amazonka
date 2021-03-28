{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.RetentionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.RetentionConfiguration
  ( RetentionConfiguration (..)
  -- * Smart constructor
  , mkRetentionConfiguration
  -- * Lenses
  , rcName
  , rcRetentionPeriodInDays
  ) where

import qualified Network.AWS.Config.Types.RetentionConfigurationName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object with the name of the retention configuration and the retention period in days. The object stores the configuration for data retention in AWS Config.
--
-- /See:/ 'mkRetentionConfiguration' smart constructor.
data RetentionConfiguration = RetentionConfiguration'
  { name :: Types.RetentionConfigurationName
    -- ^ The name of the retention configuration object.
  , retentionPeriodInDays :: Core.Natural
    -- ^ Number of days AWS Config stores your historical information.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RetentionConfiguration' value with any optional fields omitted.
mkRetentionConfiguration
    :: Types.RetentionConfigurationName -- ^ 'name'
    -> Core.Natural -- ^ 'retentionPeriodInDays'
    -> RetentionConfiguration
mkRetentionConfiguration name retentionPeriodInDays
  = RetentionConfiguration'{name, retentionPeriodInDays}

-- | The name of the retention configuration object.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcName :: Lens.Lens' RetentionConfiguration Types.RetentionConfigurationName
rcName = Lens.field @"name"
{-# INLINEABLE rcName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Number of days AWS Config stores your historical information.
--
-- /Note:/ Consider using 'retentionPeriodInDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcRetentionPeriodInDays :: Lens.Lens' RetentionConfiguration Core.Natural
rcRetentionPeriodInDays = Lens.field @"retentionPeriodInDays"
{-# INLINEABLE rcRetentionPeriodInDays #-}
{-# DEPRECATED retentionPeriodInDays "Use generic-lens or generic-optics with 'retentionPeriodInDays' instead"  #-}

instance Core.FromJSON RetentionConfiguration where
        parseJSON
          = Core.withObject "RetentionConfiguration" Core.$
              \ x ->
                RetentionConfiguration' Core.<$>
                  (x Core..: "Name") Core.<*> x Core..: "RetentionPeriodInDays"
