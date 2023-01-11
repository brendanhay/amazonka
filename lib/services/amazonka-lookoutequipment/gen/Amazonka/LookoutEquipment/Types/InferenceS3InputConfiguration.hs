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
-- Module      : Amazonka.LookoutEquipment.Types.InferenceS3InputConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.InferenceS3InputConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies configuration information for the input data for the
-- inference, including input data S3 location.
--
-- /See:/ 'newInferenceS3InputConfiguration' smart constructor.
data InferenceS3InputConfiguration = InferenceS3InputConfiguration'
  { -- | The prefix for the S3 bucket used for the input data for the inference.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | The bucket containing the input dataset for the inference.
    bucket :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InferenceS3InputConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefix', 'inferenceS3InputConfiguration_prefix' - The prefix for the S3 bucket used for the input data for the inference.
--
-- 'bucket', 'inferenceS3InputConfiguration_bucket' - The bucket containing the input dataset for the inference.
newInferenceS3InputConfiguration ::
  -- | 'bucket'
  Prelude.Text ->
  InferenceS3InputConfiguration
newInferenceS3InputConfiguration pBucket_ =
  InferenceS3InputConfiguration'
    { prefix =
        Prelude.Nothing,
      bucket = pBucket_
    }

-- | The prefix for the S3 bucket used for the input data for the inference.
inferenceS3InputConfiguration_prefix :: Lens.Lens' InferenceS3InputConfiguration (Prelude.Maybe Prelude.Text)
inferenceS3InputConfiguration_prefix = Lens.lens (\InferenceS3InputConfiguration' {prefix} -> prefix) (\s@InferenceS3InputConfiguration' {} a -> s {prefix = a} :: InferenceS3InputConfiguration)

-- | The bucket containing the input dataset for the inference.
inferenceS3InputConfiguration_bucket :: Lens.Lens' InferenceS3InputConfiguration Prelude.Text
inferenceS3InputConfiguration_bucket = Lens.lens (\InferenceS3InputConfiguration' {bucket} -> bucket) (\s@InferenceS3InputConfiguration' {} a -> s {bucket = a} :: InferenceS3InputConfiguration)

instance Data.FromJSON InferenceS3InputConfiguration where
  parseJSON =
    Data.withObject
      "InferenceS3InputConfiguration"
      ( \x ->
          InferenceS3InputConfiguration'
            Prelude.<$> (x Data..:? "Prefix")
            Prelude.<*> (x Data..: "Bucket")
      )

instance
  Prelude.Hashable
    InferenceS3InputConfiguration
  where
  hashWithSalt _salt InferenceS3InputConfiguration' {..} =
    _salt `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` bucket

instance Prelude.NFData InferenceS3InputConfiguration where
  rnf InferenceS3InputConfiguration' {..} =
    Prelude.rnf prefix `Prelude.seq` Prelude.rnf bucket

instance Data.ToJSON InferenceS3InputConfiguration where
  toJSON InferenceS3InputConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Prefix" Data..=) Prelude.<$> prefix,
            Prelude.Just ("Bucket" Data..= bucket)
          ]
      )
