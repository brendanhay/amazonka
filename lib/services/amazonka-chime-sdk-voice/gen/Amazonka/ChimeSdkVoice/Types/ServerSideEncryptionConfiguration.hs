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
-- Module      : Amazonka.ChimeSdkVoice.Types.ServerSideEncryptionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.ServerSideEncryptionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains the configuration settings for server-side
-- encryption.
--
-- We only support symmetric keys. Do not use asymmetric or HMAC keys, or
-- KMS aliases.
--
-- /See:/ 'newServerSideEncryptionConfiguration' smart constructor.
data ServerSideEncryptionConfiguration = ServerSideEncryptionConfiguration'
  { -- | The ARN of the KMS key used to encrypt the enrollment data in a voice
    -- profile domain. Asymmetric customer managed keys are not supported.
    kmsKeyArn :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServerSideEncryptionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyArn', 'serverSideEncryptionConfiguration_kmsKeyArn' - The ARN of the KMS key used to encrypt the enrollment data in a voice
-- profile domain. Asymmetric customer managed keys are not supported.
newServerSideEncryptionConfiguration ::
  -- | 'kmsKeyArn'
  Prelude.Text ->
  ServerSideEncryptionConfiguration
newServerSideEncryptionConfiguration pKmsKeyArn_ =
  ServerSideEncryptionConfiguration'
    { kmsKeyArn =
        Data._Sensitive Lens.# pKmsKeyArn_
    }

-- | The ARN of the KMS key used to encrypt the enrollment data in a voice
-- profile domain. Asymmetric customer managed keys are not supported.
serverSideEncryptionConfiguration_kmsKeyArn :: Lens.Lens' ServerSideEncryptionConfiguration Prelude.Text
serverSideEncryptionConfiguration_kmsKeyArn = Lens.lens (\ServerSideEncryptionConfiguration' {kmsKeyArn} -> kmsKeyArn) (\s@ServerSideEncryptionConfiguration' {} a -> s {kmsKeyArn = a} :: ServerSideEncryptionConfiguration) Prelude.. Data._Sensitive

instance
  Data.FromJSON
    ServerSideEncryptionConfiguration
  where
  parseJSON =
    Data.withObject
      "ServerSideEncryptionConfiguration"
      ( \x ->
          ServerSideEncryptionConfiguration'
            Prelude.<$> (x Data..: "KmsKeyArn")
      )

instance
  Prelude.Hashable
    ServerSideEncryptionConfiguration
  where
  hashWithSalt
    _salt
    ServerSideEncryptionConfiguration' {..} =
      _salt `Prelude.hashWithSalt` kmsKeyArn

instance
  Prelude.NFData
    ServerSideEncryptionConfiguration
  where
  rnf ServerSideEncryptionConfiguration' {..} =
    Prelude.rnf kmsKeyArn

instance
  Data.ToJSON
    ServerSideEncryptionConfiguration
  where
  toJSON ServerSideEncryptionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("KmsKeyArn" Data..= kmsKeyArn)]
      )
