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
-- Module      : Amazonka.OpenSearch.Types.EncryptionAtRestOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.EncryptionAtRestOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies encryption at rest options.
--
-- /See:/ 'newEncryptionAtRestOptions' smart constructor.
data EncryptionAtRestOptions = EncryptionAtRestOptions'
  { -- | The option to enable encryption at rest.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The KMS key ID for encryption at rest options.
    kmsKeyId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EncryptionAtRestOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'encryptionAtRestOptions_enabled' - The option to enable encryption at rest.
--
-- 'kmsKeyId', 'encryptionAtRestOptions_kmsKeyId' - The KMS key ID for encryption at rest options.
newEncryptionAtRestOptions ::
  EncryptionAtRestOptions
newEncryptionAtRestOptions =
  EncryptionAtRestOptions'
    { enabled = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing
    }

-- | The option to enable encryption at rest.
encryptionAtRestOptions_enabled :: Lens.Lens' EncryptionAtRestOptions (Prelude.Maybe Prelude.Bool)
encryptionAtRestOptions_enabled = Lens.lens (\EncryptionAtRestOptions' {enabled} -> enabled) (\s@EncryptionAtRestOptions' {} a -> s {enabled = a} :: EncryptionAtRestOptions)

-- | The KMS key ID for encryption at rest options.
encryptionAtRestOptions_kmsKeyId :: Lens.Lens' EncryptionAtRestOptions (Prelude.Maybe Prelude.Text)
encryptionAtRestOptions_kmsKeyId = Lens.lens (\EncryptionAtRestOptions' {kmsKeyId} -> kmsKeyId) (\s@EncryptionAtRestOptions' {} a -> s {kmsKeyId = a} :: EncryptionAtRestOptions)

instance Core.FromJSON EncryptionAtRestOptions where
  parseJSON =
    Core.withObject
      "EncryptionAtRestOptions"
      ( \x ->
          EncryptionAtRestOptions'
            Prelude.<$> (x Core..:? "Enabled")
            Prelude.<*> (x Core..:? "KmsKeyId")
      )

instance Prelude.Hashable EncryptionAtRestOptions where
  hashWithSalt salt' EncryptionAtRestOptions' {..} =
    salt' `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData EncryptionAtRestOptions where
  rnf EncryptionAtRestOptions' {..} =
    Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf kmsKeyId

instance Core.ToJSON EncryptionAtRestOptions where
  toJSON EncryptionAtRestOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Enabled" Core..=) Prelude.<$> enabled,
            ("KmsKeyId" Core..=) Prelude.<$> kmsKeyId
          ]
      )
