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
-- Module      : Network.AWS.EKS.Types.EncryptionConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.EncryptionConfig where

import qualified Network.AWS.Core as Core
import Network.AWS.EKS.Types.Provider
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The encryption configuration for the cluster.
--
-- /See:/ 'newEncryptionConfig' smart constructor.
data EncryptionConfig = EncryptionConfig'
  { -- | Specifies the resources to be encrypted. The only supported value is
    -- \"secrets\".
    resources :: Prelude.Maybe [Prelude.Text],
    -- | Key Management Service (KMS) key. Either the ARN or the alias can be
    -- used.
    provider :: Prelude.Maybe Provider
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
-- 'resources', 'encryptionConfig_resources' - Specifies the resources to be encrypted. The only supported value is
-- \"secrets\".
--
-- 'provider', 'encryptionConfig_provider' - Key Management Service (KMS) key. Either the ARN or the alias can be
-- used.
newEncryptionConfig ::
  EncryptionConfig
newEncryptionConfig =
  EncryptionConfig'
    { resources = Prelude.Nothing,
      provider = Prelude.Nothing
    }

-- | Specifies the resources to be encrypted. The only supported value is
-- \"secrets\".
encryptionConfig_resources :: Lens.Lens' EncryptionConfig (Prelude.Maybe [Prelude.Text])
encryptionConfig_resources = Lens.lens (\EncryptionConfig' {resources} -> resources) (\s@EncryptionConfig' {} a -> s {resources = a} :: EncryptionConfig) Prelude.. Lens.mapping Lens._Coerce

-- | Key Management Service (KMS) key. Either the ARN or the alias can be
-- used.
encryptionConfig_provider :: Lens.Lens' EncryptionConfig (Prelude.Maybe Provider)
encryptionConfig_provider = Lens.lens (\EncryptionConfig' {provider} -> provider) (\s@EncryptionConfig' {} a -> s {provider = a} :: EncryptionConfig)

instance Core.FromJSON EncryptionConfig where
  parseJSON =
    Core.withObject
      "EncryptionConfig"
      ( \x ->
          EncryptionConfig'
            Prelude.<$> (x Core..:? "resources" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "provider")
      )

instance Prelude.Hashable EncryptionConfig

instance Prelude.NFData EncryptionConfig

instance Core.ToJSON EncryptionConfig where
  toJSON EncryptionConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("resources" Core..=) Prelude.<$> resources,
            ("provider" Core..=) Prelude.<$> provider
          ]
      )
