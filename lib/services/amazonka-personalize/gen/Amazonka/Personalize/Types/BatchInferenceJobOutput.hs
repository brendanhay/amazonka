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
-- Module      : Amazonka.Personalize.Types.BatchInferenceJobOutput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.BatchInferenceJobOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types.S3DataConfig
import qualified Amazonka.Prelude as Prelude

-- | The output configuration parameters of a batch inference job.
--
-- /See:/ 'newBatchInferenceJobOutput' smart constructor.
data BatchInferenceJobOutput = BatchInferenceJobOutput'
  { -- | Information on the Amazon S3 bucket in which the batch inference job\'s
    -- output is stored.
    s3DataDestination :: S3DataConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchInferenceJobOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3DataDestination', 'batchInferenceJobOutput_s3DataDestination' - Information on the Amazon S3 bucket in which the batch inference job\'s
-- output is stored.
newBatchInferenceJobOutput ::
  -- | 's3DataDestination'
  S3DataConfig ->
  BatchInferenceJobOutput
newBatchInferenceJobOutput pS3DataDestination_ =
  BatchInferenceJobOutput'
    { s3DataDestination =
        pS3DataDestination_
    }

-- | Information on the Amazon S3 bucket in which the batch inference job\'s
-- output is stored.
batchInferenceJobOutput_s3DataDestination :: Lens.Lens' BatchInferenceJobOutput S3DataConfig
batchInferenceJobOutput_s3DataDestination = Lens.lens (\BatchInferenceJobOutput' {s3DataDestination} -> s3DataDestination) (\s@BatchInferenceJobOutput' {} a -> s {s3DataDestination = a} :: BatchInferenceJobOutput)

instance Data.FromJSON BatchInferenceJobOutput where
  parseJSON =
    Data.withObject
      "BatchInferenceJobOutput"
      ( \x ->
          BatchInferenceJobOutput'
            Prelude.<$> (x Data..: "s3DataDestination")
      )

instance Prelude.Hashable BatchInferenceJobOutput where
  hashWithSalt _salt BatchInferenceJobOutput' {..} =
    _salt `Prelude.hashWithSalt` s3DataDestination

instance Prelude.NFData BatchInferenceJobOutput where
  rnf BatchInferenceJobOutput' {..} =
    Prelude.rnf s3DataDestination

instance Data.ToJSON BatchInferenceJobOutput where
  toJSON BatchInferenceJobOutput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("s3DataDestination" Data..= s3DataDestination)
          ]
      )
