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
-- Module      : Amazonka.LookoutEquipment.Types.InferenceS3OutputConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.InferenceS3OutputConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies configuration information for the output results from the
-- inference, including output S3 location.
--
-- /See:/ 'newInferenceS3OutputConfiguration' smart constructor.
data InferenceS3OutputConfiguration = InferenceS3OutputConfiguration'
  { -- | The prefix for the S3 bucket used for the output results from the
    -- inference.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | The bucket containing the output results from the inference
    bucket :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InferenceS3OutputConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefix', 'inferenceS3OutputConfiguration_prefix' - The prefix for the S3 bucket used for the output results from the
-- inference.
--
-- 'bucket', 'inferenceS3OutputConfiguration_bucket' - The bucket containing the output results from the inference
newInferenceS3OutputConfiguration ::
  -- | 'bucket'
  Prelude.Text ->
  InferenceS3OutputConfiguration
newInferenceS3OutputConfiguration pBucket_ =
  InferenceS3OutputConfiguration'
    { prefix =
        Prelude.Nothing,
      bucket = pBucket_
    }

-- | The prefix for the S3 bucket used for the output results from the
-- inference.
inferenceS3OutputConfiguration_prefix :: Lens.Lens' InferenceS3OutputConfiguration (Prelude.Maybe Prelude.Text)
inferenceS3OutputConfiguration_prefix = Lens.lens (\InferenceS3OutputConfiguration' {prefix} -> prefix) (\s@InferenceS3OutputConfiguration' {} a -> s {prefix = a} :: InferenceS3OutputConfiguration)

-- | The bucket containing the output results from the inference
inferenceS3OutputConfiguration_bucket :: Lens.Lens' InferenceS3OutputConfiguration Prelude.Text
inferenceS3OutputConfiguration_bucket = Lens.lens (\InferenceS3OutputConfiguration' {bucket} -> bucket) (\s@InferenceS3OutputConfiguration' {} a -> s {bucket = a} :: InferenceS3OutputConfiguration)

instance Data.FromJSON InferenceS3OutputConfiguration where
  parseJSON =
    Data.withObject
      "InferenceS3OutputConfiguration"
      ( \x ->
          InferenceS3OutputConfiguration'
            Prelude.<$> (x Data..:? "Prefix")
            Prelude.<*> (x Data..: "Bucket")
      )

instance
  Prelude.Hashable
    InferenceS3OutputConfiguration
  where
  hashWithSalt
    _salt
    InferenceS3OutputConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` prefix
        `Prelude.hashWithSalt` bucket

instance
  Prelude.NFData
    InferenceS3OutputConfiguration
  where
  rnf InferenceS3OutputConfiguration' {..} =
    Prelude.rnf prefix `Prelude.seq` Prelude.rnf bucket

instance Data.ToJSON InferenceS3OutputConfiguration where
  toJSON InferenceS3OutputConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Prefix" Data..=) Prelude.<$> prefix,
            Prelude.Just ("Bucket" Data..= bucket)
          ]
      )
