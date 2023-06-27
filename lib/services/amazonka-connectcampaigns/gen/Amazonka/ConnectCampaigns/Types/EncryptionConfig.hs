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
-- Module      : Amazonka.ConnectCampaigns.Types.EncryptionConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCampaigns.Types.EncryptionConfig where

import Amazonka.ConnectCampaigns.Types.EncryptionType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Encryption config for Connect Instance. Note that sensitive data will
-- always be encrypted. If disabled, service will perform encryption with
-- its own key. If enabled, a KMS key id needs to be provided and KMS
-- charges will apply. KMS is only type supported
--
-- /See:/ 'newEncryptionConfig' smart constructor.
data EncryptionConfig = EncryptionConfig'
  { encryptionType :: Prelude.Maybe EncryptionType,
    keyArn :: Prelude.Maybe Prelude.Text,
    enabled :: Prelude.Bool
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
-- 'encryptionType', 'encryptionConfig_encryptionType' - Undocumented member.
--
-- 'keyArn', 'encryptionConfig_keyArn' - Undocumented member.
--
-- 'enabled', 'encryptionConfig_enabled' - Undocumented member.
newEncryptionConfig ::
  -- | 'enabled'
  Prelude.Bool ->
  EncryptionConfig
newEncryptionConfig pEnabled_ =
  EncryptionConfig'
    { encryptionType = Prelude.Nothing,
      keyArn = Prelude.Nothing,
      enabled = pEnabled_
    }

-- | Undocumented member.
encryptionConfig_encryptionType :: Lens.Lens' EncryptionConfig (Prelude.Maybe EncryptionType)
encryptionConfig_encryptionType = Lens.lens (\EncryptionConfig' {encryptionType} -> encryptionType) (\s@EncryptionConfig' {} a -> s {encryptionType = a} :: EncryptionConfig)

-- | Undocumented member.
encryptionConfig_keyArn :: Lens.Lens' EncryptionConfig (Prelude.Maybe Prelude.Text)
encryptionConfig_keyArn = Lens.lens (\EncryptionConfig' {keyArn} -> keyArn) (\s@EncryptionConfig' {} a -> s {keyArn = a} :: EncryptionConfig)

-- | Undocumented member.
encryptionConfig_enabled :: Lens.Lens' EncryptionConfig Prelude.Bool
encryptionConfig_enabled = Lens.lens (\EncryptionConfig' {enabled} -> enabled) (\s@EncryptionConfig' {} a -> s {enabled = a} :: EncryptionConfig)

instance Data.FromJSON EncryptionConfig where
  parseJSON =
    Data.withObject
      "EncryptionConfig"
      ( \x ->
          EncryptionConfig'
            Prelude.<$> (x Data..:? "encryptionType")
            Prelude.<*> (x Data..:? "keyArn")
            Prelude.<*> (x Data..: "enabled")
      )

instance Prelude.Hashable EncryptionConfig where
  hashWithSalt _salt EncryptionConfig' {..} =
    _salt
      `Prelude.hashWithSalt` encryptionType
      `Prelude.hashWithSalt` keyArn
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData EncryptionConfig where
  rnf EncryptionConfig' {..} =
    Prelude.rnf encryptionType
      `Prelude.seq` Prelude.rnf keyArn
      `Prelude.seq` Prelude.rnf enabled

instance Data.ToJSON EncryptionConfig where
  toJSON EncryptionConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("encryptionType" Data..=)
              Prelude.<$> encryptionType,
            ("keyArn" Data..=) Prelude.<$> keyArn,
            Prelude.Just ("enabled" Data..= enabled)
          ]
      )
