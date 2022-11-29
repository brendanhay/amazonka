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
-- Module      : Amazonka.Personalize.Types.BatchInferenceJobInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.BatchInferenceJobInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Personalize.Types.S3DataConfig
import qualified Amazonka.Prelude as Prelude

-- | The input configuration of a batch inference job.
--
-- /See:/ 'newBatchInferenceJobInput' smart constructor.
data BatchInferenceJobInput = BatchInferenceJobInput'
  { -- | The URI of the Amazon S3 location that contains your input data. The
    -- Amazon S3 bucket must be in the same region as the API endpoint you are
    -- calling.
    s3DataSource :: S3DataConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchInferenceJobInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3DataSource', 'batchInferenceJobInput_s3DataSource' - The URI of the Amazon S3 location that contains your input data. The
-- Amazon S3 bucket must be in the same region as the API endpoint you are
-- calling.
newBatchInferenceJobInput ::
  -- | 's3DataSource'
  S3DataConfig ->
  BatchInferenceJobInput
newBatchInferenceJobInput pS3DataSource_ =
  BatchInferenceJobInput'
    { s3DataSource =
        pS3DataSource_
    }

-- | The URI of the Amazon S3 location that contains your input data. The
-- Amazon S3 bucket must be in the same region as the API endpoint you are
-- calling.
batchInferenceJobInput_s3DataSource :: Lens.Lens' BatchInferenceJobInput S3DataConfig
batchInferenceJobInput_s3DataSource = Lens.lens (\BatchInferenceJobInput' {s3DataSource} -> s3DataSource) (\s@BatchInferenceJobInput' {} a -> s {s3DataSource = a} :: BatchInferenceJobInput)

instance Core.FromJSON BatchInferenceJobInput where
  parseJSON =
    Core.withObject
      "BatchInferenceJobInput"
      ( \x ->
          BatchInferenceJobInput'
            Prelude.<$> (x Core..: "s3DataSource")
      )

instance Prelude.Hashable BatchInferenceJobInput where
  hashWithSalt _salt BatchInferenceJobInput' {..} =
    _salt `Prelude.hashWithSalt` s3DataSource

instance Prelude.NFData BatchInferenceJobInput where
  rnf BatchInferenceJobInput' {..} =
    Prelude.rnf s3DataSource

instance Core.ToJSON BatchInferenceJobInput where
  toJSON BatchInferenceJobInput' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("s3DataSource" Core..= s3DataSource)]
      )
