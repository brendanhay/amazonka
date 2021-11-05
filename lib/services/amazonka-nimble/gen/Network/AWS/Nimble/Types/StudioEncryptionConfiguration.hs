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
-- Module      : Network.AWS.Nimble.Types.StudioEncryptionConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Nimble.Types.StudioEncryptionConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Nimble.Types.StudioEncryptionConfigurationKeyType
import qualified Network.AWS.Prelude as Prelude

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

instance Core.FromJSON StudioEncryptionConfiguration where
  parseJSON =
    Core.withObject
      "StudioEncryptionConfiguration"
      ( \x ->
          StudioEncryptionConfiguration'
            Prelude.<$> (x Core..:? "keyArn")
            Prelude.<*> (x Core..: "keyType")
      )

instance
  Prelude.Hashable
    StudioEncryptionConfiguration

instance Prelude.NFData StudioEncryptionConfiguration

instance Core.ToJSON StudioEncryptionConfiguration where
  toJSON StudioEncryptionConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("keyArn" Core..=) Prelude.<$> keyArn,
            Prelude.Just ("keyType" Core..= keyType)
          ]
      )
