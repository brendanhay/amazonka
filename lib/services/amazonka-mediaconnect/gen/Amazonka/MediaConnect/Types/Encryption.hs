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
-- Module      : Amazonka.MediaConnect.Types.Encryption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.Encryption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.Algorithm
import Amazonka.MediaConnect.Types.KeyType
import qualified Amazonka.Prelude as Prelude

-- | Information about the encryption of the flow.
--
-- /See:/ 'newEncryption' smart constructor.
data Encryption = Encryption'
  { -- | The type of algorithm that is used for the encryption (such as aes128,
    -- aes192, or aes256).
    algorithm :: Prelude.Maybe Algorithm,
    -- | A 128-bit, 16-byte hex value represented by a 32-character string, to be
    -- used with the key for encrypting content. This parameter is not valid
    -- for static key encryption.
    constantInitializationVector :: Prelude.Maybe Prelude.Text,
    -- | The value of one of the devices that you configured with your digital
    -- rights management (DRM) platform key provider. This parameter is
    -- required for SPEKE encryption and is not valid for static key
    -- encryption.
    deviceId :: Prelude.Maybe Prelude.Text,
    -- | The type of key that is used for the encryption. If no keyType is
    -- provided, the service will use the default setting (static-key).
    keyType :: Prelude.Maybe KeyType,
    -- | The AWS Region that the API Gateway proxy endpoint was created in. This
    -- parameter is required for SPEKE encryption and is not valid for static
    -- key encryption.
    region :: Prelude.Maybe Prelude.Text,
    -- | An identifier for the content. The service sends this value to the key
    -- server to identify the current endpoint. The resource ID is also known
    -- as the content ID. This parameter is required for SPEKE encryption and
    -- is not valid for static key encryption.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the secret that you created in AWS Secrets Manager to store
    -- the encryption key. This parameter is required for static key encryption
    -- and is not valid for SPEKE encryption.
    secretArn :: Prelude.Maybe Prelude.Text,
    -- | The URL from the API Gateway proxy that you set up to talk to your key
    -- server. This parameter is required for SPEKE encryption and is not valid
    -- for static key encryption.
    url :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the role that you created during setup (when you set up AWS
    -- Elemental MediaConnect as a trusted entity).
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Encryption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'algorithm', 'encryption_algorithm' - The type of algorithm that is used for the encryption (such as aes128,
-- aes192, or aes256).
--
-- 'constantInitializationVector', 'encryption_constantInitializationVector' - A 128-bit, 16-byte hex value represented by a 32-character string, to be
-- used with the key for encrypting content. This parameter is not valid
-- for static key encryption.
--
-- 'deviceId', 'encryption_deviceId' - The value of one of the devices that you configured with your digital
-- rights management (DRM) platform key provider. This parameter is
-- required for SPEKE encryption and is not valid for static key
-- encryption.
--
-- 'keyType', 'encryption_keyType' - The type of key that is used for the encryption. If no keyType is
-- provided, the service will use the default setting (static-key).
--
-- 'region', 'encryption_region' - The AWS Region that the API Gateway proxy endpoint was created in. This
-- parameter is required for SPEKE encryption and is not valid for static
-- key encryption.
--
-- 'resourceId', 'encryption_resourceId' - An identifier for the content. The service sends this value to the key
-- server to identify the current endpoint. The resource ID is also known
-- as the content ID. This parameter is required for SPEKE encryption and
-- is not valid for static key encryption.
--
-- 'secretArn', 'encryption_secretArn' - The ARN of the secret that you created in AWS Secrets Manager to store
-- the encryption key. This parameter is required for static key encryption
-- and is not valid for SPEKE encryption.
--
-- 'url', 'encryption_url' - The URL from the API Gateway proxy that you set up to talk to your key
-- server. This parameter is required for SPEKE encryption and is not valid
-- for static key encryption.
--
-- 'roleArn', 'encryption_roleArn' - The ARN of the role that you created during setup (when you set up AWS
-- Elemental MediaConnect as a trusted entity).
newEncryption ::
  -- | 'roleArn'
  Prelude.Text ->
  Encryption
newEncryption pRoleArn_ =
  Encryption'
    { algorithm = Prelude.Nothing,
      constantInitializationVector = Prelude.Nothing,
      deviceId = Prelude.Nothing,
      keyType = Prelude.Nothing,
      region = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      secretArn = Prelude.Nothing,
      url = Prelude.Nothing,
      roleArn = pRoleArn_
    }

-- | The type of algorithm that is used for the encryption (such as aes128,
-- aes192, or aes256).
encryption_algorithm :: Lens.Lens' Encryption (Prelude.Maybe Algorithm)
encryption_algorithm = Lens.lens (\Encryption' {algorithm} -> algorithm) (\s@Encryption' {} a -> s {algorithm = a} :: Encryption)

-- | A 128-bit, 16-byte hex value represented by a 32-character string, to be
-- used with the key for encrypting content. This parameter is not valid
-- for static key encryption.
encryption_constantInitializationVector :: Lens.Lens' Encryption (Prelude.Maybe Prelude.Text)
encryption_constantInitializationVector = Lens.lens (\Encryption' {constantInitializationVector} -> constantInitializationVector) (\s@Encryption' {} a -> s {constantInitializationVector = a} :: Encryption)

-- | The value of one of the devices that you configured with your digital
-- rights management (DRM) platform key provider. This parameter is
-- required for SPEKE encryption and is not valid for static key
-- encryption.
encryption_deviceId :: Lens.Lens' Encryption (Prelude.Maybe Prelude.Text)
encryption_deviceId = Lens.lens (\Encryption' {deviceId} -> deviceId) (\s@Encryption' {} a -> s {deviceId = a} :: Encryption)

-- | The type of key that is used for the encryption. If no keyType is
-- provided, the service will use the default setting (static-key).
encryption_keyType :: Lens.Lens' Encryption (Prelude.Maybe KeyType)
encryption_keyType = Lens.lens (\Encryption' {keyType} -> keyType) (\s@Encryption' {} a -> s {keyType = a} :: Encryption)

-- | The AWS Region that the API Gateway proxy endpoint was created in. This
-- parameter is required for SPEKE encryption and is not valid for static
-- key encryption.
encryption_region :: Lens.Lens' Encryption (Prelude.Maybe Prelude.Text)
encryption_region = Lens.lens (\Encryption' {region} -> region) (\s@Encryption' {} a -> s {region = a} :: Encryption)

-- | An identifier for the content. The service sends this value to the key
-- server to identify the current endpoint. The resource ID is also known
-- as the content ID. This parameter is required for SPEKE encryption and
-- is not valid for static key encryption.
encryption_resourceId :: Lens.Lens' Encryption (Prelude.Maybe Prelude.Text)
encryption_resourceId = Lens.lens (\Encryption' {resourceId} -> resourceId) (\s@Encryption' {} a -> s {resourceId = a} :: Encryption)

-- | The ARN of the secret that you created in AWS Secrets Manager to store
-- the encryption key. This parameter is required for static key encryption
-- and is not valid for SPEKE encryption.
encryption_secretArn :: Lens.Lens' Encryption (Prelude.Maybe Prelude.Text)
encryption_secretArn = Lens.lens (\Encryption' {secretArn} -> secretArn) (\s@Encryption' {} a -> s {secretArn = a} :: Encryption)

-- | The URL from the API Gateway proxy that you set up to talk to your key
-- server. This parameter is required for SPEKE encryption and is not valid
-- for static key encryption.
encryption_url :: Lens.Lens' Encryption (Prelude.Maybe Prelude.Text)
encryption_url = Lens.lens (\Encryption' {url} -> url) (\s@Encryption' {} a -> s {url = a} :: Encryption)

-- | The ARN of the role that you created during setup (when you set up AWS
-- Elemental MediaConnect as a trusted entity).
encryption_roleArn :: Lens.Lens' Encryption Prelude.Text
encryption_roleArn = Lens.lens (\Encryption' {roleArn} -> roleArn) (\s@Encryption' {} a -> s {roleArn = a} :: Encryption)

instance Data.FromJSON Encryption where
  parseJSON =
    Data.withObject
      "Encryption"
      ( \x ->
          Encryption'
            Prelude.<$> (x Data..:? "algorithm")
            Prelude.<*> (x Data..:? "constantInitializationVector")
            Prelude.<*> (x Data..:? "deviceId")
            Prelude.<*> (x Data..:? "keyType")
            Prelude.<*> (x Data..:? "region")
            Prelude.<*> (x Data..:? "resourceId")
            Prelude.<*> (x Data..:? "secretArn")
            Prelude.<*> (x Data..:? "url")
            Prelude.<*> (x Data..: "roleArn")
      )

instance Prelude.Hashable Encryption where
  hashWithSalt _salt Encryption' {..} =
    _salt `Prelude.hashWithSalt` algorithm
      `Prelude.hashWithSalt` constantInitializationVector
      `Prelude.hashWithSalt` deviceId
      `Prelude.hashWithSalt` keyType
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` secretArn
      `Prelude.hashWithSalt` url
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData Encryption where
  rnf Encryption' {..} =
    Prelude.rnf algorithm
      `Prelude.seq` Prelude.rnf constantInitializationVector
      `Prelude.seq` Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf keyType
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf secretArn
      `Prelude.seq` Prelude.rnf url
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToJSON Encryption where
  toJSON Encryption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("algorithm" Data..=) Prelude.<$> algorithm,
            ("constantInitializationVector" Data..=)
              Prelude.<$> constantInitializationVector,
            ("deviceId" Data..=) Prelude.<$> deviceId,
            ("keyType" Data..=) Prelude.<$> keyType,
            ("region" Data..=) Prelude.<$> region,
            ("resourceId" Data..=) Prelude.<$> resourceId,
            ("secretArn" Data..=) Prelude.<$> secretArn,
            ("url" Data..=) Prelude.<$> url,
            Prelude.Just ("roleArn" Data..= roleArn)
          ]
      )
