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
-- Module      : Amazonka.LookoutMetrics.Types.DetectedS3SourceConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.DetectedS3SourceConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types.DetectedFileFormatDescriptor
import qualified Amazonka.Prelude as Prelude

-- | An inferred source configuration.
--
-- /See:/ 'newDetectedS3SourceConfig' smart constructor.
data DetectedS3SourceConfig = DetectedS3SourceConfig'
  { -- | The source\'s file format descriptor.
    fileFormatDescriptor :: Prelude.Maybe DetectedFileFormatDescriptor
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectedS3SourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileFormatDescriptor', 'detectedS3SourceConfig_fileFormatDescriptor' - The source\'s file format descriptor.
newDetectedS3SourceConfig ::
  DetectedS3SourceConfig
newDetectedS3SourceConfig =
  DetectedS3SourceConfig'
    { fileFormatDescriptor =
        Prelude.Nothing
    }

-- | The source\'s file format descriptor.
detectedS3SourceConfig_fileFormatDescriptor :: Lens.Lens' DetectedS3SourceConfig (Prelude.Maybe DetectedFileFormatDescriptor)
detectedS3SourceConfig_fileFormatDescriptor = Lens.lens (\DetectedS3SourceConfig' {fileFormatDescriptor} -> fileFormatDescriptor) (\s@DetectedS3SourceConfig' {} a -> s {fileFormatDescriptor = a} :: DetectedS3SourceConfig)

instance Data.FromJSON DetectedS3SourceConfig where
  parseJSON =
    Data.withObject
      "DetectedS3SourceConfig"
      ( \x ->
          DetectedS3SourceConfig'
            Prelude.<$> (x Data..:? "FileFormatDescriptor")
      )

instance Prelude.Hashable DetectedS3SourceConfig where
  hashWithSalt _salt DetectedS3SourceConfig' {..} =
    _salt `Prelude.hashWithSalt` fileFormatDescriptor

instance Prelude.NFData DetectedS3SourceConfig where
  rnf DetectedS3SourceConfig' {..} =
    Prelude.rnf fileFormatDescriptor
