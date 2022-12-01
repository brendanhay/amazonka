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
-- Module      : Amazonka.Personalize.Types.BatchSegmentJobOutput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.BatchSegmentJobOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Personalize.Types.S3DataConfig
import qualified Amazonka.Prelude as Prelude

-- | The output configuration parameters of a batch segment job.
--
-- /See:/ 'newBatchSegmentJobOutput' smart constructor.
data BatchSegmentJobOutput = BatchSegmentJobOutput'
  { s3DataDestination :: S3DataConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchSegmentJobOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3DataDestination', 'batchSegmentJobOutput_s3DataDestination' - Undocumented member.
newBatchSegmentJobOutput ::
  -- | 's3DataDestination'
  S3DataConfig ->
  BatchSegmentJobOutput
newBatchSegmentJobOutput pS3DataDestination_ =
  BatchSegmentJobOutput'
    { s3DataDestination =
        pS3DataDestination_
    }

-- | Undocumented member.
batchSegmentJobOutput_s3DataDestination :: Lens.Lens' BatchSegmentJobOutput S3DataConfig
batchSegmentJobOutput_s3DataDestination = Lens.lens (\BatchSegmentJobOutput' {s3DataDestination} -> s3DataDestination) (\s@BatchSegmentJobOutput' {} a -> s {s3DataDestination = a} :: BatchSegmentJobOutput)

instance Core.FromJSON BatchSegmentJobOutput where
  parseJSON =
    Core.withObject
      "BatchSegmentJobOutput"
      ( \x ->
          BatchSegmentJobOutput'
            Prelude.<$> (x Core..: "s3DataDestination")
      )

instance Prelude.Hashable BatchSegmentJobOutput where
  hashWithSalt _salt BatchSegmentJobOutput' {..} =
    _salt `Prelude.hashWithSalt` s3DataDestination

instance Prelude.NFData BatchSegmentJobOutput where
  rnf BatchSegmentJobOutput' {..} =
    Prelude.rnf s3DataDestination

instance Core.ToJSON BatchSegmentJobOutput where
  toJSON BatchSegmentJobOutput' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("s3DataDestination" Core..= s3DataDestination)
          ]
      )
