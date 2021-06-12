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

-- | The encryption configuration for the cluster.
--
-- /See:/ 'newEncryptionConfig' smart constructor.
data EncryptionConfig = EncryptionConfig'
  { -- | Specifies the resources to be encrypted. The only supported value is
    -- \"secrets\".
    resources :: Core.Maybe [Core.Text],
    -- | AWS Key Management Service (AWS KMS) customer master key (CMK). Either
    -- the ARN or the alias can be used.
    provider :: Core.Maybe Provider
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
-- 'resources', 'encryptionConfig_resources' - Specifies the resources to be encrypted. The only supported value is
-- \"secrets\".
--
-- 'provider', 'encryptionConfig_provider' - AWS Key Management Service (AWS KMS) customer master key (CMK). Either
-- the ARN or the alias can be used.
newEncryptionConfig ::
  EncryptionConfig
newEncryptionConfig =
  EncryptionConfig'
    { resources = Core.Nothing,
      provider = Core.Nothing
    }

-- | Specifies the resources to be encrypted. The only supported value is
-- \"secrets\".
encryptionConfig_resources :: Lens.Lens' EncryptionConfig (Core.Maybe [Core.Text])
encryptionConfig_resources = Lens.lens (\EncryptionConfig' {resources} -> resources) (\s@EncryptionConfig' {} a -> s {resources = a} :: EncryptionConfig) Core.. Lens.mapping Lens._Coerce

-- | AWS Key Management Service (AWS KMS) customer master key (CMK). Either
-- the ARN or the alias can be used.
encryptionConfig_provider :: Lens.Lens' EncryptionConfig (Core.Maybe Provider)
encryptionConfig_provider = Lens.lens (\EncryptionConfig' {provider} -> provider) (\s@EncryptionConfig' {} a -> s {provider = a} :: EncryptionConfig)

instance Core.FromJSON EncryptionConfig where
  parseJSON =
    Core.withObject
      "EncryptionConfig"
      ( \x ->
          EncryptionConfig'
            Core.<$> (x Core..:? "resources" Core..!= Core.mempty)
            Core.<*> (x Core..:? "provider")
      )

instance Core.Hashable EncryptionConfig

instance Core.NFData EncryptionConfig

instance Core.ToJSON EncryptionConfig where
  toJSON EncryptionConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("resources" Core..=) Core.<$> resources,
            ("provider" Core..=) Core.<$> provider
          ]
      )
