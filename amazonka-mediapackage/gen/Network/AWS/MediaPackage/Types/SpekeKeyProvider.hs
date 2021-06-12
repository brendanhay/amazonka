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
-- Module      : Network.AWS.MediaPackage.Types.SpekeKeyProvider
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.SpekeKeyProvider where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A configuration for accessing an external Secure Packager and Encoder
-- Key Exchange (SPEKE) service that will provide encryption keys.
--
-- /See:/ 'newSpekeKeyProvider' smart constructor.
data SpekeKeyProvider = SpekeKeyProvider'
  { -- | An Amazon Resource Name (ARN) of a Certificate Manager certificate that
    -- MediaPackage will use for enforcing secure end-to-end data transfer with
    -- the key provider service.
    certificateArn :: Core.Maybe Core.Text,
    -- | The resource ID to include in key requests.
    resourceId :: Core.Text,
    -- | The system IDs to include in key requests.
    systemIds :: [Core.Text],
    -- | The URL of the external key provider service.
    url :: Core.Text,
    -- | An Amazon Resource Name (ARN) of an IAM role that AWS Elemental
    -- MediaPackage will assume when accessing the key provider service.
    roleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'url'
  Core.Text ->
  -- | 'roleArn'
  Core.Text ->
  SpekeKeyProvider
newSpekeKeyProvider pResourceId_ pUrl_ pRoleArn_ =
  SpekeKeyProvider'
    { certificateArn = Core.Nothing,
      resourceId = pResourceId_,
      systemIds = Core.mempty,
      url = pUrl_,
      roleArn = pRoleArn_
    }

-- | An Amazon Resource Name (ARN) of a Certificate Manager certificate that
-- MediaPackage will use for enforcing secure end-to-end data transfer with
-- the key provider service.
spekeKeyProvider_certificateArn :: Lens.Lens' SpekeKeyProvider (Core.Maybe Core.Text)
spekeKeyProvider_certificateArn = Lens.lens (\SpekeKeyProvider' {certificateArn} -> certificateArn) (\s@SpekeKeyProvider' {} a -> s {certificateArn = a} :: SpekeKeyProvider)

-- | The resource ID to include in key requests.
spekeKeyProvider_resourceId :: Lens.Lens' SpekeKeyProvider Core.Text
spekeKeyProvider_resourceId = Lens.lens (\SpekeKeyProvider' {resourceId} -> resourceId) (\s@SpekeKeyProvider' {} a -> s {resourceId = a} :: SpekeKeyProvider)

-- | The system IDs to include in key requests.
spekeKeyProvider_systemIds :: Lens.Lens' SpekeKeyProvider [Core.Text]
spekeKeyProvider_systemIds = Lens.lens (\SpekeKeyProvider' {systemIds} -> systemIds) (\s@SpekeKeyProvider' {} a -> s {systemIds = a} :: SpekeKeyProvider) Core.. Lens._Coerce

-- | The URL of the external key provider service.
spekeKeyProvider_url :: Lens.Lens' SpekeKeyProvider Core.Text
spekeKeyProvider_url = Lens.lens (\SpekeKeyProvider' {url} -> url) (\s@SpekeKeyProvider' {} a -> s {url = a} :: SpekeKeyProvider)

-- | An Amazon Resource Name (ARN) of an IAM role that AWS Elemental
-- MediaPackage will assume when accessing the key provider service.
spekeKeyProvider_roleArn :: Lens.Lens' SpekeKeyProvider Core.Text
spekeKeyProvider_roleArn = Lens.lens (\SpekeKeyProvider' {roleArn} -> roleArn) (\s@SpekeKeyProvider' {} a -> s {roleArn = a} :: SpekeKeyProvider)

instance Core.FromJSON SpekeKeyProvider where
  parseJSON =
    Core.withObject
      "SpekeKeyProvider"
      ( \x ->
          SpekeKeyProvider'
            Core.<$> (x Core..:? "certificateArn")
            Core.<*> (x Core..: "resourceId")
            Core.<*> (x Core..:? "systemIds" Core..!= Core.mempty)
            Core.<*> (x Core..: "url")
            Core.<*> (x Core..: "roleArn")
      )

instance Core.Hashable SpekeKeyProvider

instance Core.NFData SpekeKeyProvider

instance Core.ToJSON SpekeKeyProvider where
  toJSON SpekeKeyProvider' {..} =
    Core.object
      ( Core.catMaybes
          [ ("certificateArn" Core..=) Core.<$> certificateArn,
            Core.Just ("resourceId" Core..= resourceId),
            Core.Just ("systemIds" Core..= systemIds),
            Core.Just ("url" Core..= url),
            Core.Just ("roleArn" Core..= roleArn)
          ]
      )
