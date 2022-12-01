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
-- Module      : Amazonka.Nimble.Types.StreamingImageEncryptionConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.StreamingImageEncryptionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Nimble.Types.StreamingImageEncryptionConfigurationKeyType
import qualified Amazonka.Prelude as Prelude

-- | Specifies how a streaming image is encrypted.
--
-- /See:/ 'newStreamingImageEncryptionConfiguration' smart constructor.
data StreamingImageEncryptionConfiguration = StreamingImageEncryptionConfiguration'
  { -- | The ARN for a KMS key that is used to encrypt studio data.
    keyArn :: Prelude.Maybe Prelude.Text,
    -- | The type of KMS key that is used to encrypt studio data.
    keyType :: StreamingImageEncryptionConfigurationKeyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamingImageEncryptionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyArn', 'streamingImageEncryptionConfiguration_keyArn' - The ARN for a KMS key that is used to encrypt studio data.
--
-- 'keyType', 'streamingImageEncryptionConfiguration_keyType' - The type of KMS key that is used to encrypt studio data.
newStreamingImageEncryptionConfiguration ::
  -- | 'keyType'
  StreamingImageEncryptionConfigurationKeyType ->
  StreamingImageEncryptionConfiguration
newStreamingImageEncryptionConfiguration pKeyType_ =
  StreamingImageEncryptionConfiguration'
    { keyArn =
        Prelude.Nothing,
      keyType = pKeyType_
    }

-- | The ARN for a KMS key that is used to encrypt studio data.
streamingImageEncryptionConfiguration_keyArn :: Lens.Lens' StreamingImageEncryptionConfiguration (Prelude.Maybe Prelude.Text)
streamingImageEncryptionConfiguration_keyArn = Lens.lens (\StreamingImageEncryptionConfiguration' {keyArn} -> keyArn) (\s@StreamingImageEncryptionConfiguration' {} a -> s {keyArn = a} :: StreamingImageEncryptionConfiguration)

-- | The type of KMS key that is used to encrypt studio data.
streamingImageEncryptionConfiguration_keyType :: Lens.Lens' StreamingImageEncryptionConfiguration StreamingImageEncryptionConfigurationKeyType
streamingImageEncryptionConfiguration_keyType = Lens.lens (\StreamingImageEncryptionConfiguration' {keyType} -> keyType) (\s@StreamingImageEncryptionConfiguration' {} a -> s {keyType = a} :: StreamingImageEncryptionConfiguration)

instance
  Core.FromJSON
    StreamingImageEncryptionConfiguration
  where
  parseJSON =
    Core.withObject
      "StreamingImageEncryptionConfiguration"
      ( \x ->
          StreamingImageEncryptionConfiguration'
            Prelude.<$> (x Core..:? "keyArn")
            Prelude.<*> (x Core..: "keyType")
      )

instance
  Prelude.Hashable
    StreamingImageEncryptionConfiguration
  where
  hashWithSalt
    _salt
    StreamingImageEncryptionConfiguration' {..} =
      _salt `Prelude.hashWithSalt` keyArn
        `Prelude.hashWithSalt` keyType

instance
  Prelude.NFData
    StreamingImageEncryptionConfiguration
  where
  rnf StreamingImageEncryptionConfiguration' {..} =
    Prelude.rnf keyArn
      `Prelude.seq` Prelude.rnf keyType
