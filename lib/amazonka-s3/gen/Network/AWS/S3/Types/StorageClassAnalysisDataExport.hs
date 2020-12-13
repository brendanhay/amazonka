{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.StorageClassAnalysisDataExport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.StorageClassAnalysisDataExport
  ( StorageClassAnalysisDataExport (..),

    -- * Smart constructor
    mkStorageClassAnalysisDataExport,

    -- * Lenses
    scadeOutputSchemaVersion,
    scadeDestination,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.AnalyticsExportDestination
import Network.AWS.S3.Types.StorageClassAnalysisSchemaVersion

-- | Container for data related to the storage class analysis for an Amazon S3 bucket for export.
--
-- /See:/ 'mkStorageClassAnalysisDataExport' smart constructor.
data StorageClassAnalysisDataExport = StorageClassAnalysisDataExport'
  { -- | The version of the output schema to use when exporting data. Must be @V_1@ .
    outputSchemaVersion :: StorageClassAnalysisSchemaVersion,
    -- | The place to store the data for an analysis.
    destination :: AnalyticsExportDestination
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StorageClassAnalysisDataExport' with the minimum fields required to make a request.
--
-- * 'outputSchemaVersion' - The version of the output schema to use when exporting data. Must be @V_1@ .
-- * 'destination' - The place to store the data for an analysis.
mkStorageClassAnalysisDataExport ::
  -- | 'outputSchemaVersion'
  StorageClassAnalysisSchemaVersion ->
  -- | 'destination'
  AnalyticsExportDestination ->
  StorageClassAnalysisDataExport
mkStorageClassAnalysisDataExport
  pOutputSchemaVersion_
  pDestination_ =
    StorageClassAnalysisDataExport'
      { outputSchemaVersion =
          pOutputSchemaVersion_,
        destination = pDestination_
      }

-- | The version of the output schema to use when exporting data. Must be @V_1@ .
--
-- /Note:/ Consider using 'outputSchemaVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scadeOutputSchemaVersion :: Lens.Lens' StorageClassAnalysisDataExport StorageClassAnalysisSchemaVersion
scadeOutputSchemaVersion = Lens.lens (outputSchemaVersion :: StorageClassAnalysisDataExport -> StorageClassAnalysisSchemaVersion) (\s a -> s {outputSchemaVersion = a} :: StorageClassAnalysisDataExport)
{-# DEPRECATED scadeOutputSchemaVersion "Use generic-lens or generic-optics with 'outputSchemaVersion' instead." #-}

-- | The place to store the data for an analysis.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scadeDestination :: Lens.Lens' StorageClassAnalysisDataExport AnalyticsExportDestination
scadeDestination = Lens.lens (destination :: StorageClassAnalysisDataExport -> AnalyticsExportDestination) (\s a -> s {destination = a} :: StorageClassAnalysisDataExport)
{-# DEPRECATED scadeDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

instance Lude.FromXML StorageClassAnalysisDataExport where
  parseXML x =
    StorageClassAnalysisDataExport'
      Lude.<$> (x Lude..@ "OutputSchemaVersion")
      Lude.<*> (x Lude..@ "Destination")

instance Lude.ToXML StorageClassAnalysisDataExport where
  toXML StorageClassAnalysisDataExport' {..} =
    Lude.mconcat
      [ "OutputSchemaVersion" Lude.@= outputSchemaVersion,
        "Destination" Lude.@= destination
      ]
