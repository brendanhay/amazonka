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
-- Module      : Amazonka.Forecast.Types.EncryptionConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.EncryptionConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An AWS Key Management Service (KMS) key and an AWS Identity and Access
-- Management (IAM) role that Amazon Forecast can assume to access the key.
-- You can specify this optional object in the CreateDataset and
-- CreatePredictor requests.
--
-- /See:/ 'newEncryptionConfig' smart constructor.
data EncryptionConfig = EncryptionConfig'
  { -- | The ARN of the IAM role that Amazon Forecast can assume to access the
    -- AWS KMS key.
    --
    -- Passing a role across AWS accounts is not allowed. If you pass a role
    -- that isn\'t in your account, you get an @InvalidInputException@ error.
    roleArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the KMS key.
    kmsKeyArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EncryptionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'encryptionConfig_roleArn' - The ARN of the IAM role that Amazon Forecast can assume to access the
-- AWS KMS key.
--
-- Passing a role across AWS accounts is not allowed. If you pass a role
-- that isn\'t in your account, you get an @InvalidInputException@ error.
--
-- 'kmsKeyArn', 'encryptionConfig_kmsKeyArn' - The Amazon Resource Name (ARN) of the KMS key.
newEncryptionConfig ::
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'kmsKeyArn'
  Prelude.Text ->
  EncryptionConfig
newEncryptionConfig pRoleArn_ pKMSKeyArn_ =
  EncryptionConfig'
    { roleArn = pRoleArn_,
      kmsKeyArn = pKMSKeyArn_
    }

-- | The ARN of the IAM role that Amazon Forecast can assume to access the
-- AWS KMS key.
--
-- Passing a role across AWS accounts is not allowed. If you pass a role
-- that isn\'t in your account, you get an @InvalidInputException@ error.
encryptionConfig_roleArn :: Lens.Lens' EncryptionConfig Prelude.Text
encryptionConfig_roleArn = Lens.lens (\EncryptionConfig' {roleArn} -> roleArn) (\s@EncryptionConfig' {} a -> s {roleArn = a} :: EncryptionConfig)

-- | The Amazon Resource Name (ARN) of the KMS key.
encryptionConfig_kmsKeyArn :: Lens.Lens' EncryptionConfig Prelude.Text
encryptionConfig_kmsKeyArn = Lens.lens (\EncryptionConfig' {kmsKeyArn} -> kmsKeyArn) (\s@EncryptionConfig' {} a -> s {kmsKeyArn = a} :: EncryptionConfig)

instance Core.FromJSON EncryptionConfig where
  parseJSON =
    Core.withObject
      "EncryptionConfig"
      ( \x ->
          EncryptionConfig'
            Prelude.<$> (x Core..: "RoleArn")
            Prelude.<*> (x Core..: "KMSKeyArn")
      )

instance Prelude.Hashable EncryptionConfig where
  hashWithSalt _salt EncryptionConfig' {..} =
    _salt `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` kmsKeyArn

instance Prelude.NFData EncryptionConfig where
  rnf EncryptionConfig' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf kmsKeyArn

instance Core.ToJSON EncryptionConfig where
  toJSON EncryptionConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("RoleArn" Core..= roleArn),
            Prelude.Just ("KMSKeyArn" Core..= kmsKeyArn)
          ]
      )
