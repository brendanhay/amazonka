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

import qualified Network.AWS.Core as Core
import Network.AWS.Firehose.Types.KMSEncryptionConfig
import Network.AWS.Firehose.Types.NoEncryptionConfig
import qualified Network.AWS.Lens as Lens

-- | Describes the encryption for a destination in Amazon S3.
--
-- /See:/ 'newEncryptionConfiguration' smart constructor.
data EncryptionConfiguration = EncryptionConfiguration'
  { -- | The encryption key.
    kmsEncryptionConfig :: Core.Maybe KMSEncryptionConfig,
    -- | Specifically override existing encryption information to ensure that no
    -- encryption is used.
    noEncryptionConfig :: Core.Maybe NoEncryptionConfig
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      noEncryptionConfig = Core.Nothing
    }

-- | The encryption key.
encryptionConfiguration_kmsEncryptionConfig :: Lens.Lens' EncryptionConfiguration (Core.Maybe KMSEncryptionConfig)
encryptionConfiguration_kmsEncryptionConfig = Lens.lens (\EncryptionConfiguration' {kmsEncryptionConfig} -> kmsEncryptionConfig) (\s@EncryptionConfiguration' {} a -> s {kmsEncryptionConfig = a} :: EncryptionConfiguration)

-- | Specifically override existing encryption information to ensure that no
-- encryption is used.
encryptionConfiguration_noEncryptionConfig :: Lens.Lens' EncryptionConfiguration (Core.Maybe NoEncryptionConfig)
encryptionConfiguration_noEncryptionConfig = Lens.lens (\EncryptionConfiguration' {noEncryptionConfig} -> noEncryptionConfig) (\s@EncryptionConfiguration' {} a -> s {noEncryptionConfig = a} :: EncryptionConfiguration)

instance Core.FromJSON EncryptionConfiguration where
  parseJSON =
    Core.withObject
      "EncryptionConfiguration"
      ( \x ->
          EncryptionConfiguration'
            Core.<$> (x Core..:? "KMSEncryptionConfig")
            Core.<*> (x Core..:? "NoEncryptionConfig")
      )

instance Core.Hashable EncryptionConfiguration

instance Core.NFData EncryptionConfiguration

instance Core.ToJSON EncryptionConfiguration where
  toJSON EncryptionConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("KMSEncryptionConfig" Core..=)
              Core.<$> kmsEncryptionConfig,
            ("NoEncryptionConfig" Core..=)
              Core.<$> noEncryptionConfig
          ]
      )
