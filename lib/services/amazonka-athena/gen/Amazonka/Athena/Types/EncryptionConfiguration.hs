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
-- Module      : Amazonka.Athena.Types.EncryptionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.EncryptionConfiguration where

import Amazonka.Athena.Types.EncryptionOption
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | If query and calculation results are encrypted in Amazon S3, indicates
-- the encryption option used (for example, @SSE_KMS@ or @CSE_KMS@) and key
-- information.
--
-- /See:/ 'newEncryptionConfiguration' smart constructor.
data EncryptionConfiguration = EncryptionConfiguration'
  { -- | For @SSE_KMS@ and @CSE_KMS@, this is the KMS key ARN or ID.
    kmsKey :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether Amazon S3 server-side encryption with Amazon
    -- S3-managed keys (@SSE_S3@), server-side encryption with KMS-managed keys
    -- (@SSE_KMS@), or client-side encryption with KMS-managed keys (@CSE_KMS@)
    -- is used.
    --
    -- If a query runs in a workgroup and the workgroup overrides client-side
    -- settings, then the workgroup\'s setting for encryption is used. It
    -- specifies whether query results must be encrypted, for all queries that
    -- run in this workgroup.
    encryptionOption :: EncryptionOption
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EncryptionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKey', 'encryptionConfiguration_kmsKey' - For @SSE_KMS@ and @CSE_KMS@, this is the KMS key ARN or ID.
--
-- 'encryptionOption', 'encryptionConfiguration_encryptionOption' - Indicates whether Amazon S3 server-side encryption with Amazon
-- S3-managed keys (@SSE_S3@), server-side encryption with KMS-managed keys
-- (@SSE_KMS@), or client-side encryption with KMS-managed keys (@CSE_KMS@)
-- is used.
--
-- If a query runs in a workgroup and the workgroup overrides client-side
-- settings, then the workgroup\'s setting for encryption is used. It
-- specifies whether query results must be encrypted, for all queries that
-- run in this workgroup.
newEncryptionConfiguration ::
  -- | 'encryptionOption'
  EncryptionOption ->
  EncryptionConfiguration
newEncryptionConfiguration pEncryptionOption_ =
  EncryptionConfiguration'
    { kmsKey = Prelude.Nothing,
      encryptionOption = pEncryptionOption_
    }

-- | For @SSE_KMS@ and @CSE_KMS@, this is the KMS key ARN or ID.
encryptionConfiguration_kmsKey :: Lens.Lens' EncryptionConfiguration (Prelude.Maybe Prelude.Text)
encryptionConfiguration_kmsKey = Lens.lens (\EncryptionConfiguration' {kmsKey} -> kmsKey) (\s@EncryptionConfiguration' {} a -> s {kmsKey = a} :: EncryptionConfiguration)

-- | Indicates whether Amazon S3 server-side encryption with Amazon
-- S3-managed keys (@SSE_S3@), server-side encryption with KMS-managed keys
-- (@SSE_KMS@), or client-side encryption with KMS-managed keys (@CSE_KMS@)
-- is used.
--
-- If a query runs in a workgroup and the workgroup overrides client-side
-- settings, then the workgroup\'s setting for encryption is used. It
-- specifies whether query results must be encrypted, for all queries that
-- run in this workgroup.
encryptionConfiguration_encryptionOption :: Lens.Lens' EncryptionConfiguration EncryptionOption
encryptionConfiguration_encryptionOption = Lens.lens (\EncryptionConfiguration' {encryptionOption} -> encryptionOption) (\s@EncryptionConfiguration' {} a -> s {encryptionOption = a} :: EncryptionConfiguration)

instance Data.FromJSON EncryptionConfiguration where
  parseJSON =
    Data.withObject
      "EncryptionConfiguration"
      ( \x ->
          EncryptionConfiguration'
            Prelude.<$> (x Data..:? "KmsKey")
            Prelude.<*> (x Data..: "EncryptionOption")
      )

instance Prelude.Hashable EncryptionConfiguration where
  hashWithSalt _salt EncryptionConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` kmsKey
      `Prelude.hashWithSalt` encryptionOption

instance Prelude.NFData EncryptionConfiguration where
  rnf EncryptionConfiguration' {..} =
    Prelude.rnf kmsKey
      `Prelude.seq` Prelude.rnf encryptionOption

instance Data.ToJSON EncryptionConfiguration where
  toJSON EncryptionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KmsKey" Data..=) Prelude.<$> kmsKey,
            Prelude.Just
              ("EncryptionOption" Data..= encryptionOption)
          ]
      )
