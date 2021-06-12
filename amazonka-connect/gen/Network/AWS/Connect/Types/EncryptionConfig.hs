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
-- Module      : Network.AWS.Connect.Types.EncryptionConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.EncryptionConfig where

import Network.AWS.Connect.Types.EncryptionType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The encryption configuration.
--
-- /See:/ 'newEncryptionConfig' smart constructor.
data EncryptionConfig = EncryptionConfig'
  { -- | The type of encryption.
    encryptionType :: EncryptionType,
    -- | The identifier of the encryption key.
    keyId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EncryptionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionType', 'encryptionConfig_encryptionType' - The type of encryption.
--
-- 'keyId', 'encryptionConfig_keyId' - The identifier of the encryption key.
newEncryptionConfig ::
  -- | 'encryptionType'
  EncryptionType ->
  -- | 'keyId'
  Core.Text ->
  EncryptionConfig
newEncryptionConfig pEncryptionType_ pKeyId_ =
  EncryptionConfig'
    { encryptionType =
        pEncryptionType_,
      keyId = pKeyId_
    }

-- | The type of encryption.
encryptionConfig_encryptionType :: Lens.Lens' EncryptionConfig EncryptionType
encryptionConfig_encryptionType = Lens.lens (\EncryptionConfig' {encryptionType} -> encryptionType) (\s@EncryptionConfig' {} a -> s {encryptionType = a} :: EncryptionConfig)

-- | The identifier of the encryption key.
encryptionConfig_keyId :: Lens.Lens' EncryptionConfig Core.Text
encryptionConfig_keyId = Lens.lens (\EncryptionConfig' {keyId} -> keyId) (\s@EncryptionConfig' {} a -> s {keyId = a} :: EncryptionConfig)

instance Core.FromJSON EncryptionConfig where
  parseJSON =
    Core.withObject
      "EncryptionConfig"
      ( \x ->
          EncryptionConfig'
            Core.<$> (x Core..: "EncryptionType")
            Core.<*> (x Core..: "KeyId")
      )

instance Core.Hashable EncryptionConfig

instance Core.NFData EncryptionConfig

instance Core.ToJSON EncryptionConfig where
  toJSON EncryptionConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("EncryptionType" Core..= encryptionType),
            Core.Just ("KeyId" Core..= keyId)
          ]
      )
