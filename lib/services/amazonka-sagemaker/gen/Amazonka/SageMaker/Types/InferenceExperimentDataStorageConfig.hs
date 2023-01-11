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
-- Module      : Amazonka.SageMaker.Types.InferenceExperimentDataStorageConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.InferenceExperimentDataStorageConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.CaptureContentTypeHeader

-- | The Amazon S3 location and configuration for storing inference request
-- and response data.
--
-- /See:/ 'newInferenceExperimentDataStorageConfig' smart constructor.
data InferenceExperimentDataStorageConfig = InferenceExperimentDataStorageConfig'
  { contentType :: Prelude.Maybe CaptureContentTypeHeader,
    -- | The Amazon Web Services Key Management Service key that Amazon SageMaker
    -- uses to encrypt captured data at rest using Amazon S3 server-side
    -- encryption.
    kmsKey :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 bucket where the inference request and response data is
    -- stored.
    destination :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InferenceExperimentDataStorageConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'inferenceExperimentDataStorageConfig_contentType' - Undocumented member.
--
-- 'kmsKey', 'inferenceExperimentDataStorageConfig_kmsKey' - The Amazon Web Services Key Management Service key that Amazon SageMaker
-- uses to encrypt captured data at rest using Amazon S3 server-side
-- encryption.
--
-- 'destination', 'inferenceExperimentDataStorageConfig_destination' - The Amazon S3 bucket where the inference request and response data is
-- stored.
newInferenceExperimentDataStorageConfig ::
  -- | 'destination'
  Prelude.Text ->
  InferenceExperimentDataStorageConfig
newInferenceExperimentDataStorageConfig pDestination_ =
  InferenceExperimentDataStorageConfig'
    { contentType =
        Prelude.Nothing,
      kmsKey = Prelude.Nothing,
      destination = pDestination_
    }

-- | Undocumented member.
inferenceExperimentDataStorageConfig_contentType :: Lens.Lens' InferenceExperimentDataStorageConfig (Prelude.Maybe CaptureContentTypeHeader)
inferenceExperimentDataStorageConfig_contentType = Lens.lens (\InferenceExperimentDataStorageConfig' {contentType} -> contentType) (\s@InferenceExperimentDataStorageConfig' {} a -> s {contentType = a} :: InferenceExperimentDataStorageConfig)

-- | The Amazon Web Services Key Management Service key that Amazon SageMaker
-- uses to encrypt captured data at rest using Amazon S3 server-side
-- encryption.
inferenceExperimentDataStorageConfig_kmsKey :: Lens.Lens' InferenceExperimentDataStorageConfig (Prelude.Maybe Prelude.Text)
inferenceExperimentDataStorageConfig_kmsKey = Lens.lens (\InferenceExperimentDataStorageConfig' {kmsKey} -> kmsKey) (\s@InferenceExperimentDataStorageConfig' {} a -> s {kmsKey = a} :: InferenceExperimentDataStorageConfig)

-- | The Amazon S3 bucket where the inference request and response data is
-- stored.
inferenceExperimentDataStorageConfig_destination :: Lens.Lens' InferenceExperimentDataStorageConfig Prelude.Text
inferenceExperimentDataStorageConfig_destination = Lens.lens (\InferenceExperimentDataStorageConfig' {destination} -> destination) (\s@InferenceExperimentDataStorageConfig' {} a -> s {destination = a} :: InferenceExperimentDataStorageConfig)

instance
  Data.FromJSON
    InferenceExperimentDataStorageConfig
  where
  parseJSON =
    Data.withObject
      "InferenceExperimentDataStorageConfig"
      ( \x ->
          InferenceExperimentDataStorageConfig'
            Prelude.<$> (x Data..:? "ContentType")
            Prelude.<*> (x Data..:? "KmsKey")
            Prelude.<*> (x Data..: "Destination")
      )

instance
  Prelude.Hashable
    InferenceExperimentDataStorageConfig
  where
  hashWithSalt
    _salt
    InferenceExperimentDataStorageConfig' {..} =
      _salt `Prelude.hashWithSalt` contentType
        `Prelude.hashWithSalt` kmsKey
        `Prelude.hashWithSalt` destination

instance
  Prelude.NFData
    InferenceExperimentDataStorageConfig
  where
  rnf InferenceExperimentDataStorageConfig' {..} =
    Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf kmsKey
      `Prelude.seq` Prelude.rnf destination

instance
  Data.ToJSON
    InferenceExperimentDataStorageConfig
  where
  toJSON InferenceExperimentDataStorageConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ContentType" Data..=) Prelude.<$> contentType,
            ("KmsKey" Data..=) Prelude.<$> kmsKey,
            Prelude.Just ("Destination" Data..= destination)
          ]
      )
