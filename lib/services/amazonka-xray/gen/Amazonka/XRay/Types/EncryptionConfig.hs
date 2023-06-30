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
-- Module      : Amazonka.XRay.Types.EncryptionConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.EncryptionConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.XRay.Types.EncryptionStatus
import Amazonka.XRay.Types.EncryptionType

-- | A configuration document that specifies encryption configuration
-- settings.
--
-- /See:/ 'newEncryptionConfig' smart constructor.
data EncryptionConfig = EncryptionConfig'
  { -- | The ID of the KMS key used for encryption, if applicable.
    keyId :: Prelude.Maybe Prelude.Text,
    -- | The encryption status. While the status is @UPDATING@, X-Ray may encrypt
    -- data with a combination of the new and old settings.
    status :: Prelude.Maybe EncryptionStatus,
    -- | The type of encryption. Set to @KMS@ for encryption with KMS keys. Set
    -- to @NONE@ for default encryption.
    type' :: Prelude.Maybe EncryptionType
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
-- 'keyId', 'encryptionConfig_keyId' - The ID of the KMS key used for encryption, if applicable.
--
-- 'status', 'encryptionConfig_status' - The encryption status. While the status is @UPDATING@, X-Ray may encrypt
-- data with a combination of the new and old settings.
--
-- 'type'', 'encryptionConfig_type' - The type of encryption. Set to @KMS@ for encryption with KMS keys. Set
-- to @NONE@ for default encryption.
newEncryptionConfig ::
  EncryptionConfig
newEncryptionConfig =
  EncryptionConfig'
    { keyId = Prelude.Nothing,
      status = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The ID of the KMS key used for encryption, if applicable.
encryptionConfig_keyId :: Lens.Lens' EncryptionConfig (Prelude.Maybe Prelude.Text)
encryptionConfig_keyId = Lens.lens (\EncryptionConfig' {keyId} -> keyId) (\s@EncryptionConfig' {} a -> s {keyId = a} :: EncryptionConfig)

-- | The encryption status. While the status is @UPDATING@, X-Ray may encrypt
-- data with a combination of the new and old settings.
encryptionConfig_status :: Lens.Lens' EncryptionConfig (Prelude.Maybe EncryptionStatus)
encryptionConfig_status = Lens.lens (\EncryptionConfig' {status} -> status) (\s@EncryptionConfig' {} a -> s {status = a} :: EncryptionConfig)

-- | The type of encryption. Set to @KMS@ for encryption with KMS keys. Set
-- to @NONE@ for default encryption.
encryptionConfig_type :: Lens.Lens' EncryptionConfig (Prelude.Maybe EncryptionType)
encryptionConfig_type = Lens.lens (\EncryptionConfig' {type'} -> type') (\s@EncryptionConfig' {} a -> s {type' = a} :: EncryptionConfig)

instance Data.FromJSON EncryptionConfig where
  parseJSON =
    Data.withObject
      "EncryptionConfig"
      ( \x ->
          EncryptionConfig'
            Prelude.<$> (x Data..:? "KeyId")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable EncryptionConfig where
  hashWithSalt _salt EncryptionConfig' {..} =
    _salt
      `Prelude.hashWithSalt` keyId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` type'

instance Prelude.NFData EncryptionConfig where
  rnf EncryptionConfig' {..} =
    Prelude.rnf keyId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf type'
