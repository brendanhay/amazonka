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
-- Module      : Amazonka.MediaPackageV2.Types.SpekeKeyProvider
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageV2.Types.SpekeKeyProvider where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageV2.Types.DrmSystem
import Amazonka.MediaPackageV2.Types.EncryptionContractConfiguration
import qualified Amazonka.Prelude as Prelude

-- | The parameters for the SPEKE key provider.
--
-- /See:/ 'newSpekeKeyProvider' smart constructor.
data SpekeKeyProvider = SpekeKeyProvider'
  { -- | Configure one or more content encryption keys for your endpoints that
    -- use SPEKE Version 2.0. The encryption contract defines which content
    -- keys are used to encrypt the audio and video tracks in your stream. To
    -- configure the encryption contract, specify which audio and video
    -- encryption presets to use.
    encryptionContractConfiguration :: EncryptionContractConfiguration,
    -- | The unique identifier for the content. The service sends this to the key
    -- server to identify the current endpoint. How unique you make this
    -- depends on how fine-grained you want access controls to be. The service
    -- does not permit you to use the same ID for two simultaneous encryption
    -- processes. The resource ID is also known as the content ID.
    --
    -- The following example shows a resource ID: @MovieNight20171126093045@
    resourceId :: Prelude.Text,
    -- | The DRM solution provider you\'re using to protect your content during
    -- distribution.
    drmSystems :: Prelude.NonEmpty DrmSystem,
    -- | The ARN for the IAM role granted by the key provider that provides
    -- access to the key provider API. This role must have a trust policy that
    -- allows MediaPackage to assume the role, and it must have a sufficient
    -- permissions policy to allow access to the specific key retrieval URL.
    -- Get this from your DRM solution provider.
    --
    -- Valid format: @arn:aws:iam::{accountID}:role\/{name}@. The following
    -- example shows a role ARN: @arn:aws:iam::444455556666:role\/SpekeAccess@
    roleArn :: Prelude.Text,
    -- | The URL of the API Gateway proxy that you set up to talk to your key
    -- server. The API Gateway proxy must reside in the same AWS Region as
    -- MediaPackage and must start with https:\/\/.
    --
    -- The following example shows a URL:
    -- @https:\/\/1wm2dx1f33.execute-api.us-west-2.amazonaws.com\/SpekeSample\/copyProtection@
    url :: Prelude.Text
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
-- 'encryptionContractConfiguration', 'spekeKeyProvider_encryptionContractConfiguration' - Configure one or more content encryption keys for your endpoints that
-- use SPEKE Version 2.0. The encryption contract defines which content
-- keys are used to encrypt the audio and video tracks in your stream. To
-- configure the encryption contract, specify which audio and video
-- encryption presets to use.
--
-- 'resourceId', 'spekeKeyProvider_resourceId' - The unique identifier for the content. The service sends this to the key
-- server to identify the current endpoint. How unique you make this
-- depends on how fine-grained you want access controls to be. The service
-- does not permit you to use the same ID for two simultaneous encryption
-- processes. The resource ID is also known as the content ID.
--
-- The following example shows a resource ID: @MovieNight20171126093045@
--
-- 'drmSystems', 'spekeKeyProvider_drmSystems' - The DRM solution provider you\'re using to protect your content during
-- distribution.
--
-- 'roleArn', 'spekeKeyProvider_roleArn' - The ARN for the IAM role granted by the key provider that provides
-- access to the key provider API. This role must have a trust policy that
-- allows MediaPackage to assume the role, and it must have a sufficient
-- permissions policy to allow access to the specific key retrieval URL.
-- Get this from your DRM solution provider.
--
-- Valid format: @arn:aws:iam::{accountID}:role\/{name}@. The following
-- example shows a role ARN: @arn:aws:iam::444455556666:role\/SpekeAccess@
--
-- 'url', 'spekeKeyProvider_url' - The URL of the API Gateway proxy that you set up to talk to your key
-- server. The API Gateway proxy must reside in the same AWS Region as
-- MediaPackage and must start with https:\/\/.
--
-- The following example shows a URL:
-- @https:\/\/1wm2dx1f33.execute-api.us-west-2.amazonaws.com\/SpekeSample\/copyProtection@
newSpekeKeyProvider ::
  -- | 'encryptionContractConfiguration'
  EncryptionContractConfiguration ->
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'drmSystems'
  Prelude.NonEmpty DrmSystem ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'url'
  Prelude.Text ->
  SpekeKeyProvider
newSpekeKeyProvider
  pEncryptionContractConfiguration_
  pResourceId_
  pDrmSystems_
  pRoleArn_
  pUrl_ =
    SpekeKeyProvider'
      { encryptionContractConfiguration =
          pEncryptionContractConfiguration_,
        resourceId = pResourceId_,
        drmSystems = Lens.coerced Lens.# pDrmSystems_,
        roleArn = pRoleArn_,
        url = pUrl_
      }

-- | Configure one or more content encryption keys for your endpoints that
-- use SPEKE Version 2.0. The encryption contract defines which content
-- keys are used to encrypt the audio and video tracks in your stream. To
-- configure the encryption contract, specify which audio and video
-- encryption presets to use.
spekeKeyProvider_encryptionContractConfiguration :: Lens.Lens' SpekeKeyProvider EncryptionContractConfiguration
spekeKeyProvider_encryptionContractConfiguration = Lens.lens (\SpekeKeyProvider' {encryptionContractConfiguration} -> encryptionContractConfiguration) (\s@SpekeKeyProvider' {} a -> s {encryptionContractConfiguration = a} :: SpekeKeyProvider)

-- | The unique identifier for the content. The service sends this to the key
-- server to identify the current endpoint. How unique you make this
-- depends on how fine-grained you want access controls to be. The service
-- does not permit you to use the same ID for two simultaneous encryption
-- processes. The resource ID is also known as the content ID.
--
-- The following example shows a resource ID: @MovieNight20171126093045@
spekeKeyProvider_resourceId :: Lens.Lens' SpekeKeyProvider Prelude.Text
spekeKeyProvider_resourceId = Lens.lens (\SpekeKeyProvider' {resourceId} -> resourceId) (\s@SpekeKeyProvider' {} a -> s {resourceId = a} :: SpekeKeyProvider)

-- | The DRM solution provider you\'re using to protect your content during
-- distribution.
spekeKeyProvider_drmSystems :: Lens.Lens' SpekeKeyProvider (Prelude.NonEmpty DrmSystem)
spekeKeyProvider_drmSystems = Lens.lens (\SpekeKeyProvider' {drmSystems} -> drmSystems) (\s@SpekeKeyProvider' {} a -> s {drmSystems = a} :: SpekeKeyProvider) Prelude.. Lens.coerced

-- | The ARN for the IAM role granted by the key provider that provides
-- access to the key provider API. This role must have a trust policy that
-- allows MediaPackage to assume the role, and it must have a sufficient
-- permissions policy to allow access to the specific key retrieval URL.
-- Get this from your DRM solution provider.
--
-- Valid format: @arn:aws:iam::{accountID}:role\/{name}@. The following
-- example shows a role ARN: @arn:aws:iam::444455556666:role\/SpekeAccess@
spekeKeyProvider_roleArn :: Lens.Lens' SpekeKeyProvider Prelude.Text
spekeKeyProvider_roleArn = Lens.lens (\SpekeKeyProvider' {roleArn} -> roleArn) (\s@SpekeKeyProvider' {} a -> s {roleArn = a} :: SpekeKeyProvider)

-- | The URL of the API Gateway proxy that you set up to talk to your key
-- server. The API Gateway proxy must reside in the same AWS Region as
-- MediaPackage and must start with https:\/\/.
--
-- The following example shows a URL:
-- @https:\/\/1wm2dx1f33.execute-api.us-west-2.amazonaws.com\/SpekeSample\/copyProtection@
spekeKeyProvider_url :: Lens.Lens' SpekeKeyProvider Prelude.Text
spekeKeyProvider_url = Lens.lens (\SpekeKeyProvider' {url} -> url) (\s@SpekeKeyProvider' {} a -> s {url = a} :: SpekeKeyProvider)

instance Data.FromJSON SpekeKeyProvider where
  parseJSON =
    Data.withObject
      "SpekeKeyProvider"
      ( \x ->
          SpekeKeyProvider'
            Prelude.<$> (x Data..: "EncryptionContractConfiguration")
            Prelude.<*> (x Data..: "ResourceId")
            Prelude.<*> (x Data..: "DrmSystems")
            Prelude.<*> (x Data..: "RoleArn")
            Prelude.<*> (x Data..: "Url")
      )

instance Prelude.Hashable SpekeKeyProvider where
  hashWithSalt _salt SpekeKeyProvider' {..} =
    _salt
      `Prelude.hashWithSalt` encryptionContractConfiguration
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` drmSystems
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` url

instance Prelude.NFData SpekeKeyProvider where
  rnf SpekeKeyProvider' {..} =
    Prelude.rnf encryptionContractConfiguration
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf drmSystems
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf url

instance Data.ToJSON SpekeKeyProvider where
  toJSON SpekeKeyProvider' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "EncryptionContractConfiguration"
                  Data..= encryptionContractConfiguration
              ),
            Prelude.Just ("ResourceId" Data..= resourceId),
            Prelude.Just ("DrmSystems" Data..= drmSystems),
            Prelude.Just ("RoleArn" Data..= roleArn),
            Prelude.Just ("Url" Data..= url)
          ]
      )
