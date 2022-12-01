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
-- Module      : Amazonka.Synthetics.Types.ArtifactConfigInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Synthetics.Types.ArtifactConfigInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Synthetics.Types.S3EncryptionConfig

-- | A structure that contains the configuration for canary artifacts,
-- including the encryption-at-rest settings for artifacts that the canary
-- uploads to Amazon S3.
--
-- /See:/ 'newArtifactConfigInput' smart constructor.
data ArtifactConfigInput = ArtifactConfigInput'
  { -- | A structure that contains the configuration of the encryption-at-rest
    -- settings for artifacts that the canary uploads to Amazon S3. Artifact
    -- encryption functionality is available only for canaries that use
    -- Synthetics runtime version syn-nodejs-puppeteer-3.3 or later. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_artifact_encryption.html Encrypting canary artifacts>
    s3Encryption :: Prelude.Maybe S3EncryptionConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ArtifactConfigInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Encryption', 'artifactConfigInput_s3Encryption' - A structure that contains the configuration of the encryption-at-rest
-- settings for artifacts that the canary uploads to Amazon S3. Artifact
-- encryption functionality is available only for canaries that use
-- Synthetics runtime version syn-nodejs-puppeteer-3.3 or later. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_artifact_encryption.html Encrypting canary artifacts>
newArtifactConfigInput ::
  ArtifactConfigInput
newArtifactConfigInput =
  ArtifactConfigInput'
    { s3Encryption =
        Prelude.Nothing
    }

-- | A structure that contains the configuration of the encryption-at-rest
-- settings for artifacts that the canary uploads to Amazon S3. Artifact
-- encryption functionality is available only for canaries that use
-- Synthetics runtime version syn-nodejs-puppeteer-3.3 or later. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_artifact_encryption.html Encrypting canary artifacts>
artifactConfigInput_s3Encryption :: Lens.Lens' ArtifactConfigInput (Prelude.Maybe S3EncryptionConfig)
artifactConfigInput_s3Encryption = Lens.lens (\ArtifactConfigInput' {s3Encryption} -> s3Encryption) (\s@ArtifactConfigInput' {} a -> s {s3Encryption = a} :: ArtifactConfigInput)

instance Prelude.Hashable ArtifactConfigInput where
  hashWithSalt _salt ArtifactConfigInput' {..} =
    _salt `Prelude.hashWithSalt` s3Encryption

instance Prelude.NFData ArtifactConfigInput where
  rnf ArtifactConfigInput' {..} =
    Prelude.rnf s3Encryption

instance Core.ToJSON ArtifactConfigInput where
  toJSON ArtifactConfigInput' {..} =
    Core.object
      ( Prelude.catMaybes
          [("S3Encryption" Core..=) Prelude.<$> s3Encryption]
      )
