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
-- Module      : Amazonka.Nimble.Types.StudioEncryptionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.StudioEncryptionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types.StudioEncryptionConfigurationKeyType
import qualified Amazonka.Prelude as Prelude

-- | Configuration of the encryption method that is used for the studio.
--
-- /See:/ 'newStudioEncryptionConfiguration' smart constructor.
data StudioEncryptionConfiguration = StudioEncryptionConfiguration'
  { -- | The ARN for a KMS key that is used to encrypt studio data.
    keyArn :: Prelude.Maybe Prelude.Text,
    -- | The type of KMS key that is used to encrypt studio data.
    keyType :: StudioEncryptionConfigurationKeyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StudioEncryptionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyArn', 'studioEncryptionConfiguration_keyArn' - The ARN for a KMS key that is used to encrypt studio data.
--
-- 'keyType', 'studioEncryptionConfiguration_keyType' - The type of KMS key that is used to encrypt studio data.
newStudioEncryptionConfiguration ::
  -- | 'keyType'
  StudioEncryptionConfigurationKeyType ->
  StudioEncryptionConfiguration
newStudioEncryptionConfiguration pKeyType_ =
  StudioEncryptionConfiguration'
    { keyArn =
        Prelude.Nothing,
      keyType = pKeyType_
    }

-- | The ARN for a KMS key that is used to encrypt studio data.
studioEncryptionConfiguration_keyArn :: Lens.Lens' StudioEncryptionConfiguration (Prelude.Maybe Prelude.Text)
studioEncryptionConfiguration_keyArn = Lens.lens (\StudioEncryptionConfiguration' {keyArn} -> keyArn) (\s@StudioEncryptionConfiguration' {} a -> s {keyArn = a} :: StudioEncryptionConfiguration)

-- | The type of KMS key that is used to encrypt studio data.
studioEncryptionConfiguration_keyType :: Lens.Lens' StudioEncryptionConfiguration StudioEncryptionConfigurationKeyType
studioEncryptionConfiguration_keyType = Lens.lens (\StudioEncryptionConfiguration' {keyType} -> keyType) (\s@StudioEncryptionConfiguration' {} a -> s {keyType = a} :: StudioEncryptionConfiguration)

instance Data.FromJSON StudioEncryptionConfiguration where
  parseJSON =
    Data.withObject
      "StudioEncryptionConfiguration"
      ( \x ->
          StudioEncryptionConfiguration'
            Prelude.<$> (x Data..:? "keyArn")
            Prelude.<*> (x Data..: "keyType")
      )

instance
  Prelude.Hashable
    StudioEncryptionConfiguration
  where
  hashWithSalt _salt StudioEncryptionConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` keyArn
      `Prelude.hashWithSalt` keyType

instance Prelude.NFData StudioEncryptionConfiguration where
  rnf StudioEncryptionConfiguration' {..} =
    Prelude.rnf keyArn `Prelude.seq`
      Prelude.rnf keyType

instance Data.ToJSON StudioEncryptionConfiguration where
  toJSON StudioEncryptionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("keyArn" Data..=) Prelude.<$> keyArn,
            Prelude.Just ("keyType" Data..= keyType)
          ]
      )
