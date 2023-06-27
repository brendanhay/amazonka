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
-- Module      : Amazonka.EKS.Types.EncryptionConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.EncryptionConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EKS.Types.Provider
import qualified Amazonka.Prelude as Prelude

-- | The encryption configuration for the cluster.
--
-- /See:/ 'newEncryptionConfig' smart constructor.
data EncryptionConfig = EncryptionConfig'
  { -- | Key Management Service (KMS) key. Either the ARN or the alias can be
    -- used.
    provider :: Prelude.Maybe Provider,
    -- | Specifies the resources to be encrypted. The only supported value is
    -- \"secrets\".
    resources :: Prelude.Maybe [Prelude.Text]
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
-- 'provider', 'encryptionConfig_provider' - Key Management Service (KMS) key. Either the ARN or the alias can be
-- used.
--
-- 'resources', 'encryptionConfig_resources' - Specifies the resources to be encrypted. The only supported value is
-- \"secrets\".
newEncryptionConfig ::
  EncryptionConfig
newEncryptionConfig =
  EncryptionConfig'
    { provider = Prelude.Nothing,
      resources = Prelude.Nothing
    }

-- | Key Management Service (KMS) key. Either the ARN or the alias can be
-- used.
encryptionConfig_provider :: Lens.Lens' EncryptionConfig (Prelude.Maybe Provider)
encryptionConfig_provider = Lens.lens (\EncryptionConfig' {provider} -> provider) (\s@EncryptionConfig' {} a -> s {provider = a} :: EncryptionConfig)

-- | Specifies the resources to be encrypted. The only supported value is
-- \"secrets\".
encryptionConfig_resources :: Lens.Lens' EncryptionConfig (Prelude.Maybe [Prelude.Text])
encryptionConfig_resources = Lens.lens (\EncryptionConfig' {resources} -> resources) (\s@EncryptionConfig' {} a -> s {resources = a} :: EncryptionConfig) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON EncryptionConfig where
  parseJSON =
    Data.withObject
      "EncryptionConfig"
      ( \x ->
          EncryptionConfig'
            Prelude.<$> (x Data..:? "provider")
            Prelude.<*> (x Data..:? "resources" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable EncryptionConfig where
  hashWithSalt _salt EncryptionConfig' {..} =
    _salt
      `Prelude.hashWithSalt` provider
      `Prelude.hashWithSalt` resources

instance Prelude.NFData EncryptionConfig where
  rnf EncryptionConfig' {..} =
    Prelude.rnf provider
      `Prelude.seq` Prelude.rnf resources

instance Data.ToJSON EncryptionConfig where
  toJSON EncryptionConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("provider" Data..=) Prelude.<$> provider,
            ("resources" Data..=) Prelude.<$> resources
          ]
      )
