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
-- Module      : Network.AWS.ElasticSearch.Types.EncryptionAtRestOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.EncryptionAtRestOptions where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies the Encryption At Rest Options.
--
-- /See:/ 'newEncryptionAtRestOptions' smart constructor.
data EncryptionAtRestOptions = EncryptionAtRestOptions'
  { -- | Specifies the option to enable Encryption At Rest.
    enabled :: Core.Maybe Core.Bool,
    -- | Specifies the KMS Key ID for Encryption At Rest options.
    kmsKeyId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EncryptionAtRestOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'encryptionAtRestOptions_enabled' - Specifies the option to enable Encryption At Rest.
--
-- 'kmsKeyId', 'encryptionAtRestOptions_kmsKeyId' - Specifies the KMS Key ID for Encryption At Rest options.
newEncryptionAtRestOptions ::
  EncryptionAtRestOptions
newEncryptionAtRestOptions =
  EncryptionAtRestOptions'
    { enabled = Core.Nothing,
      kmsKeyId = Core.Nothing
    }

-- | Specifies the option to enable Encryption At Rest.
encryptionAtRestOptions_enabled :: Lens.Lens' EncryptionAtRestOptions (Core.Maybe Core.Bool)
encryptionAtRestOptions_enabled = Lens.lens (\EncryptionAtRestOptions' {enabled} -> enabled) (\s@EncryptionAtRestOptions' {} a -> s {enabled = a} :: EncryptionAtRestOptions)

-- | Specifies the KMS Key ID for Encryption At Rest options.
encryptionAtRestOptions_kmsKeyId :: Lens.Lens' EncryptionAtRestOptions (Core.Maybe Core.Text)
encryptionAtRestOptions_kmsKeyId = Lens.lens (\EncryptionAtRestOptions' {kmsKeyId} -> kmsKeyId) (\s@EncryptionAtRestOptions' {} a -> s {kmsKeyId = a} :: EncryptionAtRestOptions)

instance Core.FromJSON EncryptionAtRestOptions where
  parseJSON =
    Core.withObject
      "EncryptionAtRestOptions"
      ( \x ->
          EncryptionAtRestOptions'
            Core.<$> (x Core..:? "Enabled")
            Core.<*> (x Core..:? "KmsKeyId")
      )

instance Core.Hashable EncryptionAtRestOptions

instance Core.NFData EncryptionAtRestOptions

instance Core.ToJSON EncryptionAtRestOptions where
  toJSON EncryptionAtRestOptions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Enabled" Core..=) Core.<$> enabled,
            ("KmsKeyId" Core..=) Core.<$> kmsKeyId
          ]
      )
