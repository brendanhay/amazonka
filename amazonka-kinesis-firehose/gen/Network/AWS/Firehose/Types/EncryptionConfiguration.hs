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
-- Module      : Network.AWS.Firehose.Types.EncryptionConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.EncryptionConfiguration where

import Network.AWS.Firehose.Types.KMSEncryptionConfig
import Network.AWS.Firehose.Types.NoEncryptionConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON EncryptionConfiguration where
  parseJSON =
    Prelude.withObject
      "EncryptionConfiguration"
      ( \x ->
          EncryptionConfiguration'
            Prelude.<$> (x Prelude..:? "KMSEncryptionConfig")
            Prelude.<*> (x Prelude..:? "NoEncryptionConfig")
      )

instance Prelude.Hashable EncryptionConfiguration

instance Prelude.NFData EncryptionConfiguration

instance Prelude.ToJSON EncryptionConfiguration where
  toJSON EncryptionConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("KMSEncryptionConfig" Prelude..=)
              Prelude.<$> kmsEncryptionConfig,
            ("NoEncryptionConfig" Prelude..=)
              Prelude.<$> noEncryptionConfig
          ]
      )
