{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Glue.Types.MLUserDataEncryption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.MLUserDataEncryption where

import Network.AWS.Glue.Types.MLUserDataEncryptionModeString
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The encryption-at-rest settings of the transform that apply to accessing
-- user data.
--
-- /See:/ 'newMLUserDataEncryption' smart constructor.
data MLUserDataEncryption = MLUserDataEncryption'
  { -- | The ID for the customer-provided KMS key.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The encryption mode applied to user data. Valid values are:
    --
    -- -   DISABLED: encryption is disabled
    --
    -- -   SSEKMS: use of server-side encryption with AWS Key Management
    --     Service (SSE-KMS) for user data stored in Amazon S3.
    mlUserDataEncryptionMode :: MLUserDataEncryptionModeString
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MLUserDataEncryption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'mLUserDataEncryption_kmsKeyId' - The ID for the customer-provided KMS key.
--
-- 'mlUserDataEncryptionMode', 'mLUserDataEncryption_mlUserDataEncryptionMode' - The encryption mode applied to user data. Valid values are:
--
-- -   DISABLED: encryption is disabled
--
-- -   SSEKMS: use of server-side encryption with AWS Key Management
--     Service (SSE-KMS) for user data stored in Amazon S3.
newMLUserDataEncryption ::
  -- | 'mlUserDataEncryptionMode'
  MLUserDataEncryptionModeString ->
  MLUserDataEncryption
newMLUserDataEncryption pMlUserDataEncryptionMode_ =
  MLUserDataEncryption'
    { kmsKeyId = Prelude.Nothing,
      mlUserDataEncryptionMode =
        pMlUserDataEncryptionMode_
    }

-- | The ID for the customer-provided KMS key.
mLUserDataEncryption_kmsKeyId :: Lens.Lens' MLUserDataEncryption (Prelude.Maybe Prelude.Text)
mLUserDataEncryption_kmsKeyId = Lens.lens (\MLUserDataEncryption' {kmsKeyId} -> kmsKeyId) (\s@MLUserDataEncryption' {} a -> s {kmsKeyId = a} :: MLUserDataEncryption)

-- | The encryption mode applied to user data. Valid values are:
--
-- -   DISABLED: encryption is disabled
--
-- -   SSEKMS: use of server-side encryption with AWS Key Management
--     Service (SSE-KMS) for user data stored in Amazon S3.
mLUserDataEncryption_mlUserDataEncryptionMode :: Lens.Lens' MLUserDataEncryption MLUserDataEncryptionModeString
mLUserDataEncryption_mlUserDataEncryptionMode = Lens.lens (\MLUserDataEncryption' {mlUserDataEncryptionMode} -> mlUserDataEncryptionMode) (\s@MLUserDataEncryption' {} a -> s {mlUserDataEncryptionMode = a} :: MLUserDataEncryption)

instance Prelude.FromJSON MLUserDataEncryption where
  parseJSON =
    Prelude.withObject
      "MLUserDataEncryption"
      ( \x ->
          MLUserDataEncryption'
            Prelude.<$> (x Prelude..:? "KmsKeyId")
            Prelude.<*> (x Prelude..: "MlUserDataEncryptionMode")
      )

instance Prelude.Hashable MLUserDataEncryption

instance Prelude.NFData MLUserDataEncryption

instance Prelude.ToJSON MLUserDataEncryption where
  toJSON MLUserDataEncryption' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("KmsKeyId" Prelude..=) Prelude.<$> kmsKeyId,
            Prelude.Just
              ( "MlUserDataEncryptionMode"
                  Prelude..= mlUserDataEncryptionMode
              )
          ]
      )
