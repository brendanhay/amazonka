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
-- Module      : Amazonka.Braket.Types.JobCheckpointConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Braket.Types.JobCheckpointConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the output locations for job checkpoint data.
--
-- /See:/ 'newJobCheckpointConfig' smart constructor.
data JobCheckpointConfig = JobCheckpointConfig'
  { -- | (Optional) The local directory where checkpoints are written. The
    -- default directory is @\/opt\/braket\/checkpoints\/@.
    localPath :: Prelude.Maybe Prelude.Text,
    -- | Identifies the S3 path where you want Amazon Braket to store
    -- checkpoints. For example, @s3:\/\/bucket-name\/key-name-prefix@.
    s3Uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobCheckpointConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'localPath', 'jobCheckpointConfig_localPath' - (Optional) The local directory where checkpoints are written. The
-- default directory is @\/opt\/braket\/checkpoints\/@.
--
-- 's3Uri', 'jobCheckpointConfig_s3Uri' - Identifies the S3 path where you want Amazon Braket to store
-- checkpoints. For example, @s3:\/\/bucket-name\/key-name-prefix@.
newJobCheckpointConfig ::
  -- | 's3Uri'
  Prelude.Text ->
  JobCheckpointConfig
newJobCheckpointConfig pS3Uri_ =
  JobCheckpointConfig'
    { localPath = Prelude.Nothing,
      s3Uri = pS3Uri_
    }

-- | (Optional) The local directory where checkpoints are written. The
-- default directory is @\/opt\/braket\/checkpoints\/@.
jobCheckpointConfig_localPath :: Lens.Lens' JobCheckpointConfig (Prelude.Maybe Prelude.Text)
jobCheckpointConfig_localPath = Lens.lens (\JobCheckpointConfig' {localPath} -> localPath) (\s@JobCheckpointConfig' {} a -> s {localPath = a} :: JobCheckpointConfig)

-- | Identifies the S3 path where you want Amazon Braket to store
-- checkpoints. For example, @s3:\/\/bucket-name\/key-name-prefix@.
jobCheckpointConfig_s3Uri :: Lens.Lens' JobCheckpointConfig Prelude.Text
jobCheckpointConfig_s3Uri = Lens.lens (\JobCheckpointConfig' {s3Uri} -> s3Uri) (\s@JobCheckpointConfig' {} a -> s {s3Uri = a} :: JobCheckpointConfig)

instance Data.FromJSON JobCheckpointConfig where
  parseJSON =
    Data.withObject
      "JobCheckpointConfig"
      ( \x ->
          JobCheckpointConfig'
            Prelude.<$> (x Data..:? "localPath")
            Prelude.<*> (x Data..: "s3Uri")
      )

instance Prelude.Hashable JobCheckpointConfig where
  hashWithSalt _salt JobCheckpointConfig' {..} =
    _salt `Prelude.hashWithSalt` localPath
      `Prelude.hashWithSalt` s3Uri

instance Prelude.NFData JobCheckpointConfig where
  rnf JobCheckpointConfig' {..} =
    Prelude.rnf localPath
      `Prelude.seq` Prelude.rnf s3Uri

instance Data.ToJSON JobCheckpointConfig where
  toJSON JobCheckpointConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("localPath" Data..=) Prelude.<$> localPath,
            Prelude.Just ("s3Uri" Data..= s3Uri)
          ]
      )
