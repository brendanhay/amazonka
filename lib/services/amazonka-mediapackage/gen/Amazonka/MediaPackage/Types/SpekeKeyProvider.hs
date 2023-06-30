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
-- Module      : Amazonka.MediaPackage.Types.SpekeKeyProvider
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackage.Types.SpekeKeyProvider where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackage.Types.EncryptionContractConfiguration
import qualified Amazonka.Prelude as Prelude

-- | A configuration for accessing an external Secure Packager and Encoder
-- Key Exchange (SPEKE) service that will provide encryption keys.
--
-- /See:/ 'newSpekeKeyProvider' smart constructor.
data SpekeKeyProvider = SpekeKeyProvider'
  { -- | An Amazon Resource Name (ARN) of a Certificate Manager certificate that
    -- MediaPackage will use for enforcing secure end-to-end data transfer with
    -- the key provider service.
    certificateArn :: Prelude.Maybe Prelude.Text,
    encryptionContractConfiguration :: Prelude.Maybe EncryptionContractConfiguration,
    -- | The resource ID to include in key requests.
    resourceId :: Prelude.Text,
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
-- 'certificateArn', 'spekeKeyProvider_certificateArn' - An Amazon Resource Name (ARN) of a Certificate Manager certificate that
-- MediaPackage will use for enforcing secure end-to-end data transfer with
-- the key provider service.
--
-- 'encryptionContractConfiguration', 'spekeKeyProvider_encryptionContractConfiguration' - Undocumented member.
--
-- 'resourceId', 'spekeKeyProvider_resourceId' - The resource ID to include in key requests.
--
-- 'systemIds', 'spekeKeyProvider_systemIds' - The system IDs to include in key requests.
--
-- 'url', 'spekeKeyProvider_url' - The URL of the external key provider service.
--
-- 'roleArn', 'spekeKeyProvider_roleArn' - An Amazon Resource Name (ARN) of an IAM role that AWS Elemental
-- MediaPackage will assume when accessing the key provider service.
newSpekeKeyProvider ::
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'url'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  SpekeKeyProvider
newSpekeKeyProvider pResourceId_ pUrl_ pRoleArn_ =
  SpekeKeyProvider'
    { certificateArn = Prelude.Nothing,
      encryptionContractConfiguration = Prelude.Nothing,
      resourceId = pResourceId_,
      systemIds = Prelude.mempty,
      url = pUrl_,
      roleArn = pRoleArn_
    }

-- | An Amazon Resource Name (ARN) of a Certificate Manager certificate that
-- MediaPackage will use for enforcing secure end-to-end data transfer with
-- the key provider service.
spekeKeyProvider_certificateArn :: Lens.Lens' SpekeKeyProvider (Prelude.Maybe Prelude.Text)
spekeKeyProvider_certificateArn = Lens.lens (\SpekeKeyProvider' {certificateArn} -> certificateArn) (\s@SpekeKeyProvider' {} a -> s {certificateArn = a} :: SpekeKeyProvider)

-- | Undocumented member.
spekeKeyProvider_encryptionContractConfiguration :: Lens.Lens' SpekeKeyProvider (Prelude.Maybe EncryptionContractConfiguration)
spekeKeyProvider_encryptionContractConfiguration = Lens.lens (\SpekeKeyProvider' {encryptionContractConfiguration} -> encryptionContractConfiguration) (\s@SpekeKeyProvider' {} a -> s {encryptionContractConfiguration = a} :: SpekeKeyProvider)

-- | The resource ID to include in key requests.
spekeKeyProvider_resourceId :: Lens.Lens' SpekeKeyProvider Prelude.Text
spekeKeyProvider_resourceId = Lens.lens (\SpekeKeyProvider' {resourceId} -> resourceId) (\s@SpekeKeyProvider' {} a -> s {resourceId = a} :: SpekeKeyProvider)

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
            Prelude.<$> (x Data..:? "certificateArn")
            Prelude.<*> (x Data..:? "encryptionContractConfiguration")
            Prelude.<*> (x Data..: "resourceId")
            Prelude.<*> (x Data..:? "systemIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "url")
            Prelude.<*> (x Data..: "roleArn")
      )

instance Prelude.Hashable SpekeKeyProvider where
  hashWithSalt _salt SpekeKeyProvider' {..} =
    _salt
      `Prelude.hashWithSalt` certificateArn
      `Prelude.hashWithSalt` encryptionContractConfiguration
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` systemIds
      `Prelude.hashWithSalt` url
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData SpekeKeyProvider where
  rnf SpekeKeyProvider' {..} =
    Prelude.rnf certificateArn
      `Prelude.seq` Prelude.rnf encryptionContractConfiguration
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf systemIds
      `Prelude.seq` Prelude.rnf url
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToJSON SpekeKeyProvider where
  toJSON SpekeKeyProvider' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("certificateArn" Data..=)
              Prelude.<$> certificateArn,
            ("encryptionContractConfiguration" Data..=)
              Prelude.<$> encryptionContractConfiguration,
            Prelude.Just ("resourceId" Data..= resourceId),
            Prelude.Just ("systemIds" Data..= systemIds),
            Prelude.Just ("url" Data..= url),
            Prelude.Just ("roleArn" Data..= roleArn)
          ]
      )
