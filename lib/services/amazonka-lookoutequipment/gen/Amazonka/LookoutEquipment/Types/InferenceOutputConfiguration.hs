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
-- Module      : Amazonka.LookoutEquipment.Types.InferenceOutputConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.InferenceOutputConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutEquipment.Types.InferenceS3OutputConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Specifies configuration information for the output results from for the
-- inference, including KMS key ID and output S3 location.
--
-- /See:/ 'newInferenceOutputConfiguration' smart constructor.
data InferenceOutputConfiguration = InferenceOutputConfiguration'
  { -- | The ID number for the AWS KMS key used to encrypt the inference output.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Specifies configuration information for the output results from for the
    -- inference, output S3 location.
    s3OutputConfiguration :: InferenceS3OutputConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InferenceOutputConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'inferenceOutputConfiguration_kmsKeyId' - The ID number for the AWS KMS key used to encrypt the inference output.
--
-- 's3OutputConfiguration', 'inferenceOutputConfiguration_s3OutputConfiguration' - Specifies configuration information for the output results from for the
-- inference, output S3 location.
newInferenceOutputConfiguration ::
  -- | 's3OutputConfiguration'
  InferenceS3OutputConfiguration ->
  InferenceOutputConfiguration
newInferenceOutputConfiguration
  pS3OutputConfiguration_ =
    InferenceOutputConfiguration'
      { kmsKeyId =
          Prelude.Nothing,
        s3OutputConfiguration =
          pS3OutputConfiguration_
      }

-- | The ID number for the AWS KMS key used to encrypt the inference output.
inferenceOutputConfiguration_kmsKeyId :: Lens.Lens' InferenceOutputConfiguration (Prelude.Maybe Prelude.Text)
inferenceOutputConfiguration_kmsKeyId = Lens.lens (\InferenceOutputConfiguration' {kmsKeyId} -> kmsKeyId) (\s@InferenceOutputConfiguration' {} a -> s {kmsKeyId = a} :: InferenceOutputConfiguration)

-- | Specifies configuration information for the output results from for the
-- inference, output S3 location.
inferenceOutputConfiguration_s3OutputConfiguration :: Lens.Lens' InferenceOutputConfiguration InferenceS3OutputConfiguration
inferenceOutputConfiguration_s3OutputConfiguration = Lens.lens (\InferenceOutputConfiguration' {s3OutputConfiguration} -> s3OutputConfiguration) (\s@InferenceOutputConfiguration' {} a -> s {s3OutputConfiguration = a} :: InferenceOutputConfiguration)

instance Data.FromJSON InferenceOutputConfiguration where
  parseJSON =
    Data.withObject
      "InferenceOutputConfiguration"
      ( \x ->
          InferenceOutputConfiguration'
            Prelude.<$> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..: "S3OutputConfiguration")
      )

instance
  Prelude.Hashable
    InferenceOutputConfiguration
  where
  hashWithSalt _salt InferenceOutputConfiguration' {..} =
    _salt `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` s3OutputConfiguration

instance Prelude.NFData InferenceOutputConfiguration where
  rnf InferenceOutputConfiguration' {..} =
    Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf s3OutputConfiguration

instance Data.ToJSON InferenceOutputConfiguration where
  toJSON InferenceOutputConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            Prelude.Just
              ( "S3OutputConfiguration"
                  Data..= s3OutputConfiguration
              )
          ]
      )
