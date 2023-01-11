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
-- Module      : Amazonka.HealthLake.Types.KmsEncryptionConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.HealthLake.Types.KmsEncryptionConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.HealthLake.Types.CmkType
import qualified Amazonka.Prelude as Prelude

-- | The customer-managed-key(CMK) used when creating a Data Store. If a
-- customer owned key is not specified, an AWS owned key will be used for
-- encryption.
--
-- /See:/ 'newKmsEncryptionConfig' smart constructor.
data KmsEncryptionConfig = KmsEncryptionConfig'
  { -- | The KMS encryption key id\/alias used to encrypt the Data Store contents
    -- at rest.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The type of customer-managed-key(CMK) used for encyrption. The two types
    -- of supported CMKs are customer owned CMKs and AWS owned CMKs.
    cmkType :: CmkType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KmsEncryptionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'kmsEncryptionConfig_kmsKeyId' - The KMS encryption key id\/alias used to encrypt the Data Store contents
-- at rest.
--
-- 'cmkType', 'kmsEncryptionConfig_cmkType' - The type of customer-managed-key(CMK) used for encyrption. The two types
-- of supported CMKs are customer owned CMKs and AWS owned CMKs.
newKmsEncryptionConfig ::
  -- | 'cmkType'
  CmkType ->
  KmsEncryptionConfig
newKmsEncryptionConfig pCmkType_ =
  KmsEncryptionConfig'
    { kmsKeyId = Prelude.Nothing,
      cmkType = pCmkType_
    }

-- | The KMS encryption key id\/alias used to encrypt the Data Store contents
-- at rest.
kmsEncryptionConfig_kmsKeyId :: Lens.Lens' KmsEncryptionConfig (Prelude.Maybe Prelude.Text)
kmsEncryptionConfig_kmsKeyId = Lens.lens (\KmsEncryptionConfig' {kmsKeyId} -> kmsKeyId) (\s@KmsEncryptionConfig' {} a -> s {kmsKeyId = a} :: KmsEncryptionConfig)

-- | The type of customer-managed-key(CMK) used for encyrption. The two types
-- of supported CMKs are customer owned CMKs and AWS owned CMKs.
kmsEncryptionConfig_cmkType :: Lens.Lens' KmsEncryptionConfig CmkType
kmsEncryptionConfig_cmkType = Lens.lens (\KmsEncryptionConfig' {cmkType} -> cmkType) (\s@KmsEncryptionConfig' {} a -> s {cmkType = a} :: KmsEncryptionConfig)

instance Data.FromJSON KmsEncryptionConfig where
  parseJSON =
    Data.withObject
      "KmsEncryptionConfig"
      ( \x ->
          KmsEncryptionConfig'
            Prelude.<$> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..: "CmkType")
      )

instance Prelude.Hashable KmsEncryptionConfig where
  hashWithSalt _salt KmsEncryptionConfig' {..} =
    _salt `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` cmkType

instance Prelude.NFData KmsEncryptionConfig where
  rnf KmsEncryptionConfig' {..} =
    Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf cmkType

instance Data.ToJSON KmsEncryptionConfig where
  toJSON KmsEncryptionConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            Prelude.Just ("CmkType" Data..= cmkType)
          ]
      )
