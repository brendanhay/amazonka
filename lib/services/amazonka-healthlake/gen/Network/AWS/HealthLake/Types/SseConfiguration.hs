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
-- Module      : Network.AWS.HealthLake.Types.SseConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.HealthLake.Types.SseConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.HealthLake.Types.KmsEncryptionConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The server-side encryption key configuration for a customer provided
-- encryption key.
--
-- /See:/ 'newSseConfiguration' smart constructor.
data SseConfiguration = SseConfiguration'
  { -- | The KMS encryption configuration used to provide details for data
    -- encryption.
    kmsEncryptionConfig :: KmsEncryptionConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SseConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsEncryptionConfig', 'sseConfiguration_kmsEncryptionConfig' - The KMS encryption configuration used to provide details for data
-- encryption.
newSseConfiguration ::
  -- | 'kmsEncryptionConfig'
  KmsEncryptionConfig ->
  SseConfiguration
newSseConfiguration pKmsEncryptionConfig_ =
  SseConfiguration'
    { kmsEncryptionConfig =
        pKmsEncryptionConfig_
    }

-- | The KMS encryption configuration used to provide details for data
-- encryption.
sseConfiguration_kmsEncryptionConfig :: Lens.Lens' SseConfiguration KmsEncryptionConfig
sseConfiguration_kmsEncryptionConfig = Lens.lens (\SseConfiguration' {kmsEncryptionConfig} -> kmsEncryptionConfig) (\s@SseConfiguration' {} a -> s {kmsEncryptionConfig = a} :: SseConfiguration)

instance Core.FromJSON SseConfiguration where
  parseJSON =
    Core.withObject
      "SseConfiguration"
      ( \x ->
          SseConfiguration'
            Prelude.<$> (x Core..: "KmsEncryptionConfig")
      )

instance Prelude.Hashable SseConfiguration

instance Prelude.NFData SseConfiguration

instance Core.ToJSON SseConfiguration where
  toJSON SseConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("KmsEncryptionConfig" Core..= kmsEncryptionConfig)
          ]
      )
