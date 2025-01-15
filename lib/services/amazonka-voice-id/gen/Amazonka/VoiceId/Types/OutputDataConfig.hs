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
-- Module      : Amazonka.VoiceId.Types.OutputDataConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.OutputDataConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration containing output file information for a batch job.
--
-- /See:/ 'newOutputDataConfig' smart constructor.
data OutputDataConfig = OutputDataConfig'
  { -- | The identifier of the KMS key you want Voice ID to use to encrypt the
    -- output file of a speaker enrollment job\/fraudster registration job.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The S3 path of the folder where Voice ID writes the job output file. It
    -- has a @*.out@ extension. For example, if the input file name is
    -- @input-file.json@ and the output folder path is
    -- @s3:\/\/output-bucket\/output-folder@, the full output file path is
    -- @s3:\/\/output-bucket\/output-folder\/job-Id\/input-file.json.out@.
    s3Uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutputDataConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'outputDataConfig_kmsKeyId' - The identifier of the KMS key you want Voice ID to use to encrypt the
-- output file of a speaker enrollment job\/fraudster registration job.
--
-- 's3Uri', 'outputDataConfig_s3Uri' - The S3 path of the folder where Voice ID writes the job output file. It
-- has a @*.out@ extension. For example, if the input file name is
-- @input-file.json@ and the output folder path is
-- @s3:\/\/output-bucket\/output-folder@, the full output file path is
-- @s3:\/\/output-bucket\/output-folder\/job-Id\/input-file.json.out@.
newOutputDataConfig ::
  -- | 's3Uri'
  Prelude.Text ->
  OutputDataConfig
newOutputDataConfig pS3Uri_ =
  OutputDataConfig'
    { kmsKeyId = Prelude.Nothing,
      s3Uri = pS3Uri_
    }

-- | The identifier of the KMS key you want Voice ID to use to encrypt the
-- output file of a speaker enrollment job\/fraudster registration job.
outputDataConfig_kmsKeyId :: Lens.Lens' OutputDataConfig (Prelude.Maybe Prelude.Text)
outputDataConfig_kmsKeyId = Lens.lens (\OutputDataConfig' {kmsKeyId} -> kmsKeyId) (\s@OutputDataConfig' {} a -> s {kmsKeyId = a} :: OutputDataConfig)

-- | The S3 path of the folder where Voice ID writes the job output file. It
-- has a @*.out@ extension. For example, if the input file name is
-- @input-file.json@ and the output folder path is
-- @s3:\/\/output-bucket\/output-folder@, the full output file path is
-- @s3:\/\/output-bucket\/output-folder\/job-Id\/input-file.json.out@.
outputDataConfig_s3Uri :: Lens.Lens' OutputDataConfig Prelude.Text
outputDataConfig_s3Uri = Lens.lens (\OutputDataConfig' {s3Uri} -> s3Uri) (\s@OutputDataConfig' {} a -> s {s3Uri = a} :: OutputDataConfig)

instance Data.FromJSON OutputDataConfig where
  parseJSON =
    Data.withObject
      "OutputDataConfig"
      ( \x ->
          OutputDataConfig'
            Prelude.<$> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..: "S3Uri")
      )

instance Prelude.Hashable OutputDataConfig where
  hashWithSalt _salt OutputDataConfig' {..} =
    _salt
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` s3Uri

instance Prelude.NFData OutputDataConfig where
  rnf OutputDataConfig' {..} =
    Prelude.rnf kmsKeyId `Prelude.seq`
      Prelude.rnf s3Uri

instance Data.ToJSON OutputDataConfig where
  toJSON OutputDataConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            Prelude.Just ("S3Uri" Data..= s3Uri)
          ]
      )
