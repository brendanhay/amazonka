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
-- Module      : Network.AWS.EKS.Types.EncryptionConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.EncryptionConfig where

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
    -- | AWS Key Management Service (AWS KMS) customer master key (CMK). Either
    -- the ARN or the alias can be used.
    provider :: Prelude.Maybe Provider
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { resources = Prelude.Nothing,
      provider = Prelude.Nothing
    }

-- | Specifies the resources to be encrypted. The only supported value is
-- \"secrets\".
encryptionConfig_resources :: Lens.Lens' EncryptionConfig (Prelude.Maybe [Prelude.Text])
encryptionConfig_resources = Lens.lens (\EncryptionConfig' {resources} -> resources) (\s@EncryptionConfig' {} a -> s {resources = a} :: EncryptionConfig) Prelude.. Lens.mapping Prelude._Coerce

-- | AWS Key Management Service (AWS KMS) customer master key (CMK). Either
-- the ARN or the alias can be used.
encryptionConfig_provider :: Lens.Lens' EncryptionConfig (Prelude.Maybe Provider)
encryptionConfig_provider = Lens.lens (\EncryptionConfig' {provider} -> provider) (\s@EncryptionConfig' {} a -> s {provider = a} :: EncryptionConfig)

instance Prelude.FromJSON EncryptionConfig where
  parseJSON =
    Prelude.withObject
      "EncryptionConfig"
      ( \x ->
          EncryptionConfig'
            Prelude.<$> ( x Prelude..:? "resources"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "provider")
      )

instance Prelude.Hashable EncryptionConfig

instance Prelude.NFData EncryptionConfig

instance Prelude.ToJSON EncryptionConfig where
  toJSON EncryptionConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("resources" Prelude..=) Prelude.<$> resources,
            ("provider" Prelude..=) Prelude.<$> provider
          ]
      )
