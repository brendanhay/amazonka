{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.StorageClassAnalysisDataExport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.StorageClassAnalysisDataExport
  ( StorageClassAnalysisDataExport (..)
  -- * Smart constructor
  , mkStorageClassAnalysisDataExport
  -- * Lenses
  , scadeOutputSchemaVersion
  , scadeDestination
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.AnalyticsExportDestination as Types
import qualified Network.AWS.S3.Types.StorageClassAnalysisSchemaVersion as Types

-- | Container for data related to the storage class analysis for an Amazon S3 bucket for export.
--
-- /See:/ 'mkStorageClassAnalysisDataExport' smart constructor.
data StorageClassAnalysisDataExport = StorageClassAnalysisDataExport'
  { outputSchemaVersion :: Types.StorageClassAnalysisSchemaVersion
    -- ^ The version of the output schema to use when exporting data. Must be @V_1@ .
  , destination :: Types.AnalyticsExportDestination
    -- ^ The place to store the data for an analysis.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StorageClassAnalysisDataExport' value with any optional fields omitted.
mkStorageClassAnalysisDataExport
    :: Types.StorageClassAnalysisSchemaVersion -- ^ 'outputSchemaVersion'
    -> Types.AnalyticsExportDestination -- ^ 'destination'
    -> StorageClassAnalysisDataExport
mkStorageClassAnalysisDataExport outputSchemaVersion destination
  = StorageClassAnalysisDataExport'{outputSchemaVersion, destination}

-- | The version of the output schema to use when exporting data. Must be @V_1@ .
--
-- /Note:/ Consider using 'outputSchemaVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scadeOutputSchemaVersion :: Lens.Lens' StorageClassAnalysisDataExport Types.StorageClassAnalysisSchemaVersion
scadeOutputSchemaVersion = Lens.field @"outputSchemaVersion"
{-# INLINEABLE scadeOutputSchemaVersion #-}
{-# DEPRECATED outputSchemaVersion "Use generic-lens or generic-optics with 'outputSchemaVersion' instead"  #-}

-- | The place to store the data for an analysis.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scadeDestination :: Lens.Lens' StorageClassAnalysisDataExport Types.AnalyticsExportDestination
scadeDestination = Lens.field @"destination"
{-# INLINEABLE scadeDestination #-}
{-# DEPRECATED destination "Use generic-lens or generic-optics with 'destination' instead"  #-}

instance Core.ToXML StorageClassAnalysisDataExport where
        toXML StorageClassAnalysisDataExport{..}
          = Core.toXMLElement "OutputSchemaVersion" outputSchemaVersion
              Core.<> Core.toXMLElement "Destination" destination

instance Core.FromXML StorageClassAnalysisDataExport where
        parseXML x
          = StorageClassAnalysisDataExport' Core.<$>
              (x Core..@ "OutputSchemaVersion") Core.<*> x Core..@ "Destination"
