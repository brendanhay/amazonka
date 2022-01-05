{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.S3.Types.StorageClassAnalysisDataExport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.StorageClassAnalysisDataExport where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.AnalyticsExportDestination
import Amazonka.S3.Types.StorageClassAnalysisSchemaVersion

-- | Container for data related to the storage class analysis for an Amazon
-- S3 bucket for export.
--
-- /See:/ 'newStorageClassAnalysisDataExport' smart constructor.
data StorageClassAnalysisDataExport = StorageClassAnalysisDataExport'
  { -- | The version of the output schema to use when exporting data. Must be
    -- @V_1@.
    outputSchemaVersion :: StorageClassAnalysisSchemaVersion,
    -- | The place to store the data for an analysis.
    destination :: AnalyticsExportDestination
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StorageClassAnalysisDataExport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputSchemaVersion', 'storageClassAnalysisDataExport_outputSchemaVersion' - The version of the output schema to use when exporting data. Must be
-- @V_1@.
--
-- 'destination', 'storageClassAnalysisDataExport_destination' - The place to store the data for an analysis.
newStorageClassAnalysisDataExport ::
  -- | 'outputSchemaVersion'
  StorageClassAnalysisSchemaVersion ->
  -- | 'destination'
  AnalyticsExportDestination ->
  StorageClassAnalysisDataExport
newStorageClassAnalysisDataExport
  pOutputSchemaVersion_
  pDestination_ =
    StorageClassAnalysisDataExport'
      { outputSchemaVersion =
          pOutputSchemaVersion_,
        destination = pDestination_
      }

-- | The version of the output schema to use when exporting data. Must be
-- @V_1@.
storageClassAnalysisDataExport_outputSchemaVersion :: Lens.Lens' StorageClassAnalysisDataExport StorageClassAnalysisSchemaVersion
storageClassAnalysisDataExport_outputSchemaVersion = Lens.lens (\StorageClassAnalysisDataExport' {outputSchemaVersion} -> outputSchemaVersion) (\s@StorageClassAnalysisDataExport' {} a -> s {outputSchemaVersion = a} :: StorageClassAnalysisDataExport)

-- | The place to store the data for an analysis.
storageClassAnalysisDataExport_destination :: Lens.Lens' StorageClassAnalysisDataExport AnalyticsExportDestination
storageClassAnalysisDataExport_destination = Lens.lens (\StorageClassAnalysisDataExport' {destination} -> destination) (\s@StorageClassAnalysisDataExport' {} a -> s {destination = a} :: StorageClassAnalysisDataExport)

instance Core.FromXML StorageClassAnalysisDataExport where
  parseXML x =
    StorageClassAnalysisDataExport'
      Prelude.<$> (x Core..@ "OutputSchemaVersion")
      Prelude.<*> (x Core..@ "Destination")

instance
  Prelude.Hashable
    StorageClassAnalysisDataExport
  where
  hashWithSalt
    _salt
    StorageClassAnalysisDataExport' {..} =
      _salt `Prelude.hashWithSalt` outputSchemaVersion
        `Prelude.hashWithSalt` destination

instance
  Prelude.NFData
    StorageClassAnalysisDataExport
  where
  rnf StorageClassAnalysisDataExport' {..} =
    Prelude.rnf outputSchemaVersion
      `Prelude.seq` Prelude.rnf destination

instance Core.ToXML StorageClassAnalysisDataExport where
  toXML StorageClassAnalysisDataExport' {..} =
    Prelude.mconcat
      [ "OutputSchemaVersion" Core.@= outputSchemaVersion,
        "Destination" Core.@= destination
      ]
