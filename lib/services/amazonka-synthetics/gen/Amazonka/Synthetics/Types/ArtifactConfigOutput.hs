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
-- Module      : Amazonka.Synthetics.Types.ArtifactConfigOutput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Synthetics.Types.ArtifactConfigOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Synthetics.Types.S3EncryptionConfig

-- | A structure that contains the configuration for canary artifacts,
-- including the encryption-at-rest settings for artifacts that the canary
-- uploads to Amazon S3.
--
-- /See:/ 'newArtifactConfigOutput' smart constructor.
data ArtifactConfigOutput = ArtifactConfigOutput'
  { -- | A structure that contains the configuration of encryption settings for
    -- canary artifacts that are stored in Amazon S3.
    s3Encryption :: Prelude.Maybe S3EncryptionConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ArtifactConfigOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Encryption', 'artifactConfigOutput_s3Encryption' - A structure that contains the configuration of encryption settings for
-- canary artifacts that are stored in Amazon S3.
newArtifactConfigOutput ::
  ArtifactConfigOutput
newArtifactConfigOutput =
  ArtifactConfigOutput'
    { s3Encryption =
        Prelude.Nothing
    }

-- | A structure that contains the configuration of encryption settings for
-- canary artifacts that are stored in Amazon S3.
artifactConfigOutput_s3Encryption :: Lens.Lens' ArtifactConfigOutput (Prelude.Maybe S3EncryptionConfig)
artifactConfigOutput_s3Encryption = Lens.lens (\ArtifactConfigOutput' {s3Encryption} -> s3Encryption) (\s@ArtifactConfigOutput' {} a -> s {s3Encryption = a} :: ArtifactConfigOutput)

instance Data.FromJSON ArtifactConfigOutput where
  parseJSON =
    Data.withObject
      "ArtifactConfigOutput"
      ( \x ->
          ArtifactConfigOutput'
            Prelude.<$> (x Data..:? "S3Encryption")
      )

instance Prelude.Hashable ArtifactConfigOutput where
  hashWithSalt _salt ArtifactConfigOutput' {..} =
    _salt `Prelude.hashWithSalt` s3Encryption

instance Prelude.NFData ArtifactConfigOutput where
  rnf ArtifactConfigOutput' {..} =
    Prelude.rnf s3Encryption
