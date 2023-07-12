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
-- Module      : Amazonka.MediaPackageVOD.Types.SpekeKeyProvider
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageVOD.Types.SpekeKeyProvider where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageVOD.Types.EncryptionContractConfiguration
import qualified Amazonka.Prelude as Prelude

-- | A configuration for accessing an external Secure Packager and Encoder
-- Key Exchange (SPEKE) service that will provide encryption keys.
--
-- /See:/ 'newSpekeKeyProvider' smart constructor.
data SpekeKeyProvider = SpekeKeyProvider'
  { encryptionContractConfiguration :: Prelude.Maybe EncryptionContractConfiguration,
    -- | The system IDs to include in key requests.
    systemIds :: [Prelude.Text],
    -- | The URL of the external key provider service.
    url :: Prelude.Text,
    -- | An Amazon Resource Name (ARN) of an IAM role that AWS Elemental
    -- MediaPackage will assume when accessing the key provider service.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SpekeKeyProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionContractConfiguration', 'spekeKeyProvider_encryptionContractConfiguration' - Undocumented member.
--
-- 'systemIds', 'spekeKeyProvider_systemIds' - The system IDs to include in key requests.
--
-- 'url', 'spekeKeyProvider_url' - The URL of the external key provider service.
--
-- 'roleArn', 'spekeKeyProvider_roleArn' - An Amazon Resource Name (ARN) of an IAM role that AWS Elemental
-- MediaPackage will assume when accessing the key provider service.
newSpekeKeyProvider ::
  -- | 'url'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  SpekeKeyProvider
newSpekeKeyProvider pUrl_ pRoleArn_ =
  SpekeKeyProvider'
    { encryptionContractConfiguration =
        Prelude.Nothing,
      systemIds = Prelude.mempty,
      url = pUrl_,
      roleArn = pRoleArn_
    }

-- | Undocumented member.
spekeKeyProvider_encryptionContractConfiguration :: Lens.Lens' SpekeKeyProvider (Prelude.Maybe EncryptionContractConfiguration)
spekeKeyProvider_encryptionContractConfiguration = Lens.lens (\SpekeKeyProvider' {encryptionContractConfiguration} -> encryptionContractConfiguration) (\s@SpekeKeyProvider' {} a -> s {encryptionContractConfiguration = a} :: SpekeKeyProvider)

-- | The system IDs to include in key requests.
spekeKeyProvider_systemIds :: Lens.Lens' SpekeKeyProvider [Prelude.Text]
spekeKeyProvider_systemIds = Lens.lens (\SpekeKeyProvider' {systemIds} -> systemIds) (\s@SpekeKeyProvider' {} a -> s {systemIds = a} :: SpekeKeyProvider) Prelude.. Lens.coerced

-- | The URL of the external key provider service.
spekeKeyProvider_url :: Lens.Lens' SpekeKeyProvider Prelude.Text
spekeKeyProvider_url = Lens.lens (\SpekeKeyProvider' {url} -> url) (\s@SpekeKeyProvider' {} a -> s {url = a} :: SpekeKeyProvider)

-- | An Amazon Resource Name (ARN) of an IAM role that AWS Elemental
-- MediaPackage will assume when accessing the key provider service.
spekeKeyProvider_roleArn :: Lens.Lens' SpekeKeyProvider Prelude.Text
spekeKeyProvider_roleArn = Lens.lens (\SpekeKeyProvider' {roleArn} -> roleArn) (\s@SpekeKeyProvider' {} a -> s {roleArn = a} :: SpekeKeyProvider)

instance Data.FromJSON SpekeKeyProvider where
  parseJSON =
    Data.withObject
      "SpekeKeyProvider"
      ( \x ->
          SpekeKeyProvider'
            Prelude.<$> (x Data..:? "encryptionContractConfiguration")
            Prelude.<*> (x Data..:? "systemIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "url")
            Prelude.<*> (x Data..: "roleArn")
      )

instance Prelude.Hashable SpekeKeyProvider where
  hashWithSalt _salt SpekeKeyProvider' {..} =
    _salt
      `Prelude.hashWithSalt` encryptionContractConfiguration
      `Prelude.hashWithSalt` systemIds
      `Prelude.hashWithSalt` url
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData SpekeKeyProvider where
  rnf SpekeKeyProvider' {..} =
    Prelude.rnf encryptionContractConfiguration
      `Prelude.seq` Prelude.rnf systemIds
      `Prelude.seq` Prelude.rnf url
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToJSON SpekeKeyProvider where
  toJSON SpekeKeyProvider' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("encryptionContractConfiguration" Data..=)
              Prelude.<$> encryptionContractConfiguration,
            Prelude.Just ("systemIds" Data..= systemIds),
            Prelude.Just ("url" Data..= url),
            Prelude.Just ("roleArn" Data..= roleArn)
          ]
      )
