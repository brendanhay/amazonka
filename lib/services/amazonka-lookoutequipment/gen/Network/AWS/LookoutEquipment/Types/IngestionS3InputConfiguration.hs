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
-- Module      : Network.AWS.LookoutEquipment.Types.IngestionS3InputConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LookoutEquipment.Types.IngestionS3InputConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies S3 configuration information for the input data for the data
-- ingestion job.
--
-- /See:/ 'newIngestionS3InputConfiguration' smart constructor.
data IngestionS3InputConfiguration = IngestionS3InputConfiguration'
  { -- | The prefix for the S3 location being used for the input data for the
    -- data ingestion.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | The name of the S3 bucket used for the input data for the data
    -- ingestion.
    bucket :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IngestionS3InputConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefix', 'ingestionS3InputConfiguration_prefix' - The prefix for the S3 location being used for the input data for the
-- data ingestion.
--
-- 'bucket', 'ingestionS3InputConfiguration_bucket' - The name of the S3 bucket used for the input data for the data
-- ingestion.
newIngestionS3InputConfiguration ::
  -- | 'bucket'
  Prelude.Text ->
  IngestionS3InputConfiguration
newIngestionS3InputConfiguration pBucket_ =
  IngestionS3InputConfiguration'
    { prefix =
        Prelude.Nothing,
      bucket = pBucket_
    }

-- | The prefix for the S3 location being used for the input data for the
-- data ingestion.
ingestionS3InputConfiguration_prefix :: Lens.Lens' IngestionS3InputConfiguration (Prelude.Maybe Prelude.Text)
ingestionS3InputConfiguration_prefix = Lens.lens (\IngestionS3InputConfiguration' {prefix} -> prefix) (\s@IngestionS3InputConfiguration' {} a -> s {prefix = a} :: IngestionS3InputConfiguration)

-- | The name of the S3 bucket used for the input data for the data
-- ingestion.
ingestionS3InputConfiguration_bucket :: Lens.Lens' IngestionS3InputConfiguration Prelude.Text
ingestionS3InputConfiguration_bucket = Lens.lens (\IngestionS3InputConfiguration' {bucket} -> bucket) (\s@IngestionS3InputConfiguration' {} a -> s {bucket = a} :: IngestionS3InputConfiguration)

instance Core.FromJSON IngestionS3InputConfiguration where
  parseJSON =
    Core.withObject
      "IngestionS3InputConfiguration"
      ( \x ->
          IngestionS3InputConfiguration'
            Prelude.<$> (x Core..:? "Prefix")
            Prelude.<*> (x Core..: "Bucket")
      )

instance
  Prelude.Hashable
    IngestionS3InputConfiguration

instance Prelude.NFData IngestionS3InputConfiguration

instance Core.ToJSON IngestionS3InputConfiguration where
  toJSON IngestionS3InputConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Prefix" Core..=) Prelude.<$> prefix,
            Prelude.Just ("Bucket" Core..= bucket)
          ]
      )
