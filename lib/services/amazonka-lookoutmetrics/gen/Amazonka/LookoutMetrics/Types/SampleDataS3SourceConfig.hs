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
-- Module      : Amazonka.LookoutMetrics.Types.SampleDataS3SourceConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.SampleDataS3SourceConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LookoutMetrics.Types.FileFormatDescriptor
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the source configuration in Amazon S3.
--
-- /See:/ 'newSampleDataS3SourceConfig' smart constructor.
data SampleDataS3SourceConfig = SampleDataS3SourceConfig'
  { -- | An array of strings containing the list of templated paths.
    templatedPathList :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | An array of strings containing the historical set of data paths.
    historicalDataPathList :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the role.
    roleArn :: Prelude.Text,
    fileFormatDescriptor :: FileFormatDescriptor
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SampleDataS3SourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templatedPathList', 'sampleDataS3SourceConfig_templatedPathList' - An array of strings containing the list of templated paths.
--
-- 'historicalDataPathList', 'sampleDataS3SourceConfig_historicalDataPathList' - An array of strings containing the historical set of data paths.
--
-- 'roleArn', 'sampleDataS3SourceConfig_roleArn' - The Amazon Resource Name (ARN) of the role.
--
-- 'fileFormatDescriptor', 'sampleDataS3SourceConfig_fileFormatDescriptor' - Undocumented member.
newSampleDataS3SourceConfig ::
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'fileFormatDescriptor'
  FileFormatDescriptor ->
  SampleDataS3SourceConfig
newSampleDataS3SourceConfig
  pRoleArn_
  pFileFormatDescriptor_ =
    SampleDataS3SourceConfig'
      { templatedPathList =
          Prelude.Nothing,
        historicalDataPathList = Prelude.Nothing,
        roleArn = pRoleArn_,
        fileFormatDescriptor = pFileFormatDescriptor_
      }

-- | An array of strings containing the list of templated paths.
sampleDataS3SourceConfig_templatedPathList :: Lens.Lens' SampleDataS3SourceConfig (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
sampleDataS3SourceConfig_templatedPathList = Lens.lens (\SampleDataS3SourceConfig' {templatedPathList} -> templatedPathList) (\s@SampleDataS3SourceConfig' {} a -> s {templatedPathList = a} :: SampleDataS3SourceConfig) Prelude.. Lens.mapping Lens.coerced

-- | An array of strings containing the historical set of data paths.
sampleDataS3SourceConfig_historicalDataPathList :: Lens.Lens' SampleDataS3SourceConfig (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
sampleDataS3SourceConfig_historicalDataPathList = Lens.lens (\SampleDataS3SourceConfig' {historicalDataPathList} -> historicalDataPathList) (\s@SampleDataS3SourceConfig' {} a -> s {historicalDataPathList = a} :: SampleDataS3SourceConfig) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the role.
sampleDataS3SourceConfig_roleArn :: Lens.Lens' SampleDataS3SourceConfig Prelude.Text
sampleDataS3SourceConfig_roleArn = Lens.lens (\SampleDataS3SourceConfig' {roleArn} -> roleArn) (\s@SampleDataS3SourceConfig' {} a -> s {roleArn = a} :: SampleDataS3SourceConfig)

-- | Undocumented member.
sampleDataS3SourceConfig_fileFormatDescriptor :: Lens.Lens' SampleDataS3SourceConfig FileFormatDescriptor
sampleDataS3SourceConfig_fileFormatDescriptor = Lens.lens (\SampleDataS3SourceConfig' {fileFormatDescriptor} -> fileFormatDescriptor) (\s@SampleDataS3SourceConfig' {} a -> s {fileFormatDescriptor = a} :: SampleDataS3SourceConfig)

instance Prelude.Hashable SampleDataS3SourceConfig where
  hashWithSalt _salt SampleDataS3SourceConfig' {..} =
    _salt `Prelude.hashWithSalt` templatedPathList
      `Prelude.hashWithSalt` historicalDataPathList
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` fileFormatDescriptor

instance Prelude.NFData SampleDataS3SourceConfig where
  rnf SampleDataS3SourceConfig' {..} =
    Prelude.rnf templatedPathList
      `Prelude.seq` Prelude.rnf historicalDataPathList
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf fileFormatDescriptor

instance Core.ToJSON SampleDataS3SourceConfig where
  toJSON SampleDataS3SourceConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TemplatedPathList" Core..=)
              Prelude.<$> templatedPathList,
            ("HistoricalDataPathList" Core..=)
              Prelude.<$> historicalDataPathList,
            Prelude.Just ("RoleArn" Core..= roleArn),
            Prelude.Just
              ( "FileFormatDescriptor"
                  Core..= fileFormatDescriptor
              )
          ]
      )
