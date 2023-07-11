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
-- Module      : Amazonka.LookoutEquipment.Types.LabelsS3InputConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.LabelsS3InputConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The location information (prefix and bucket name) for the s3 location
-- being used for label data.
--
-- /See:/ 'newLabelsS3InputConfiguration' smart constructor.
data LabelsS3InputConfiguration = LabelsS3InputConfiguration'
  { -- | The prefix for the S3 bucket used for the label data.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | The name of the S3 bucket holding the label data.
    bucket :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LabelsS3InputConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefix', 'labelsS3InputConfiguration_prefix' - The prefix for the S3 bucket used for the label data.
--
-- 'bucket', 'labelsS3InputConfiguration_bucket' - The name of the S3 bucket holding the label data.
newLabelsS3InputConfiguration ::
  -- | 'bucket'
  Prelude.Text ->
  LabelsS3InputConfiguration
newLabelsS3InputConfiguration pBucket_ =
  LabelsS3InputConfiguration'
    { prefix =
        Prelude.Nothing,
      bucket = pBucket_
    }

-- | The prefix for the S3 bucket used for the label data.
labelsS3InputConfiguration_prefix :: Lens.Lens' LabelsS3InputConfiguration (Prelude.Maybe Prelude.Text)
labelsS3InputConfiguration_prefix = Lens.lens (\LabelsS3InputConfiguration' {prefix} -> prefix) (\s@LabelsS3InputConfiguration' {} a -> s {prefix = a} :: LabelsS3InputConfiguration)

-- | The name of the S3 bucket holding the label data.
labelsS3InputConfiguration_bucket :: Lens.Lens' LabelsS3InputConfiguration Prelude.Text
labelsS3InputConfiguration_bucket = Lens.lens (\LabelsS3InputConfiguration' {bucket} -> bucket) (\s@LabelsS3InputConfiguration' {} a -> s {bucket = a} :: LabelsS3InputConfiguration)

instance Data.FromJSON LabelsS3InputConfiguration where
  parseJSON =
    Data.withObject
      "LabelsS3InputConfiguration"
      ( \x ->
          LabelsS3InputConfiguration'
            Prelude.<$> (x Data..:? "Prefix")
            Prelude.<*> (x Data..: "Bucket")
      )

instance Prelude.Hashable LabelsS3InputConfiguration where
  hashWithSalt _salt LabelsS3InputConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` bucket

instance Prelude.NFData LabelsS3InputConfiguration where
  rnf LabelsS3InputConfiguration' {..} =
    Prelude.rnf prefix `Prelude.seq` Prelude.rnf bucket

instance Data.ToJSON LabelsS3InputConfiguration where
  toJSON LabelsS3InputConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Prefix" Data..=) Prelude.<$> prefix,
            Prelude.Just ("Bucket" Data..= bucket)
          ]
      )
