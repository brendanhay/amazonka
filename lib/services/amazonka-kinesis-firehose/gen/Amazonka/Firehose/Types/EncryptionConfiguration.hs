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
-- Module      : Amazonka.Firehose.Types.EncryptionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.EncryptionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Firehose.Types.KMSEncryptionConfig
import Amazonka.Firehose.Types.NoEncryptionConfig
import qualified Amazonka.Prelude as Prelude

-- | Describes the encryption for a destination in Amazon S3.
--
-- /See:/ 'newEncryptionConfiguration' smart constructor.
data EncryptionConfiguration = EncryptionConfiguration'
  { -- | The encryption key.
    kmsEncryptionConfig :: Prelude.Maybe KMSEncryptionConfig,
    -- | Specifically override existing encryption information to ensure that no
    -- encryption is used.
    noEncryptionConfig :: Prelude.Maybe NoEncryptionConfig
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
-- 'kmsEncryptionConfig', 'encryptionConfiguration_kmsEncryptionConfig' - The encryption key.
--
-- 'noEncryptionConfig', 'encryptionConfiguration_noEncryptionConfig' - Specifically override existing encryption information to ensure that no
-- encryption is used.
newEncryptionConfiguration ::
  EncryptionConfiguration
newEncryptionConfiguration =
  EncryptionConfiguration'
    { kmsEncryptionConfig =
        Prelude.Nothing,
      noEncryptionConfig = Prelude.Nothing
    }

-- | The encryption key.
encryptionConfiguration_kmsEncryptionConfig :: Lens.Lens' EncryptionConfiguration (Prelude.Maybe KMSEncryptionConfig)
encryptionConfiguration_kmsEncryptionConfig = Lens.lens (\EncryptionConfiguration' {kmsEncryptionConfig} -> kmsEncryptionConfig) (\s@EncryptionConfiguration' {} a -> s {kmsEncryptionConfig = a} :: EncryptionConfiguration)

-- | Specifically override existing encryption information to ensure that no
-- encryption is used.
encryptionConfiguration_noEncryptionConfig :: Lens.Lens' EncryptionConfiguration (Prelude.Maybe NoEncryptionConfig)
encryptionConfiguration_noEncryptionConfig = Lens.lens (\EncryptionConfiguration' {noEncryptionConfig} -> noEncryptionConfig) (\s@EncryptionConfiguration' {} a -> s {noEncryptionConfig = a} :: EncryptionConfiguration)

instance Data.FromJSON EncryptionConfiguration where
  parseJSON =
    Data.withObject
      "EncryptionConfiguration"
      ( \x ->
          EncryptionConfiguration'
            Prelude.<$> (x Data..:? "KMSEncryptionConfig")
            Prelude.<*> (x Data..:? "NoEncryptionConfig")
      )

instance Prelude.Hashable EncryptionConfiguration where
  hashWithSalt _salt EncryptionConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` kmsEncryptionConfig
      `Prelude.hashWithSalt` noEncryptionConfig

instance Prelude.NFData EncryptionConfiguration where
  rnf EncryptionConfiguration' {..} =
    Prelude.rnf kmsEncryptionConfig
      `Prelude.seq` Prelude.rnf noEncryptionConfig

instance Data.ToJSON EncryptionConfiguration where
  toJSON EncryptionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KMSEncryptionConfig" Data..=)
              Prelude.<$> kmsEncryptionConfig,
            ("NoEncryptionConfig" Data..=)
              Prelude.<$> noEncryptionConfig
          ]
      )
