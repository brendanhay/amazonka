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
-- Module      : Amazonka.Rekognition.Types.OutputConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.OutputConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The S3 bucket and folder location where training output is placed.
--
-- /See:/ 'newOutputConfig' smart constructor.
data OutputConfig = OutputConfig'
  { -- | The S3 bucket where training output is placed.
    s3Bucket :: Prelude.Maybe Prelude.Text,
    -- | The prefix applied to the training output files.
    s3KeyPrefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutputConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Bucket', 'outputConfig_s3Bucket' - The S3 bucket where training output is placed.
--
-- 's3KeyPrefix', 'outputConfig_s3KeyPrefix' - The prefix applied to the training output files.
newOutputConfig ::
  OutputConfig
newOutputConfig =
  OutputConfig'
    { s3Bucket = Prelude.Nothing,
      s3KeyPrefix = Prelude.Nothing
    }

-- | The S3 bucket where training output is placed.
outputConfig_s3Bucket :: Lens.Lens' OutputConfig (Prelude.Maybe Prelude.Text)
outputConfig_s3Bucket = Lens.lens (\OutputConfig' {s3Bucket} -> s3Bucket) (\s@OutputConfig' {} a -> s {s3Bucket = a} :: OutputConfig)

-- | The prefix applied to the training output files.
outputConfig_s3KeyPrefix :: Lens.Lens' OutputConfig (Prelude.Maybe Prelude.Text)
outputConfig_s3KeyPrefix = Lens.lens (\OutputConfig' {s3KeyPrefix} -> s3KeyPrefix) (\s@OutputConfig' {} a -> s {s3KeyPrefix = a} :: OutputConfig)

instance Data.FromJSON OutputConfig where
  parseJSON =
    Data.withObject
      "OutputConfig"
      ( \x ->
          OutputConfig'
            Prelude.<$> (x Data..:? "S3Bucket")
            Prelude.<*> (x Data..:? "S3KeyPrefix")
      )

instance Prelude.Hashable OutputConfig where
  hashWithSalt _salt OutputConfig' {..} =
    _salt
      `Prelude.hashWithSalt` s3Bucket
      `Prelude.hashWithSalt` s3KeyPrefix

instance Prelude.NFData OutputConfig where
  rnf OutputConfig' {..} =
    Prelude.rnf s3Bucket
      `Prelude.seq` Prelude.rnf s3KeyPrefix

instance Data.ToJSON OutputConfig where
  toJSON OutputConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("S3Bucket" Data..=) Prelude.<$> s3Bucket,
            ("S3KeyPrefix" Data..=) Prelude.<$> s3KeyPrefix
          ]
      )
