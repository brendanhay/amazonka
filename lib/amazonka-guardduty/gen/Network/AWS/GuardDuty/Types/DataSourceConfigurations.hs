{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.DataSourceConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.DataSourceConfigurations
  ( DataSourceConfigurations (..)
  -- * Smart constructor
  , mkDataSourceConfigurations
  -- * Lenses
  , dscS3Logs
  ) where

import qualified Network.AWS.GuardDuty.Types.S3LogsConfiguration as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about which data sources are enabled.
--
-- /See:/ 'mkDataSourceConfigurations' smart constructor.
newtype DataSourceConfigurations = DataSourceConfigurations'
  { s3Logs :: Core.Maybe Types.S3LogsConfiguration
    -- ^ Describes whether S3 data event logs are enabled as a data source.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DataSourceConfigurations' value with any optional fields omitted.
mkDataSourceConfigurations
    :: DataSourceConfigurations
mkDataSourceConfigurations
  = DataSourceConfigurations'{s3Logs = Core.Nothing}

-- | Describes whether S3 data event logs are enabled as a data source.
--
-- /Note:/ Consider using 's3Logs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscS3Logs :: Lens.Lens' DataSourceConfigurations (Core.Maybe Types.S3LogsConfiguration)
dscS3Logs = Lens.field @"s3Logs"
{-# INLINEABLE dscS3Logs #-}
{-# DEPRECATED s3Logs "Use generic-lens or generic-optics with 's3Logs' instead"  #-}

instance Core.FromJSON DataSourceConfigurations where
        toJSON DataSourceConfigurations{..}
          = Core.object (Core.catMaybes [("s3Logs" Core..=) Core.<$> s3Logs])
