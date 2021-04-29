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
-- Module      : Network.AWS.MediaPackage.Types.SpekeKeyProvider
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.SpekeKeyProvider where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A configuration for accessing an external Secure Packager and Encoder
-- Key Exchange (SPEKE) service that will provide encryption keys.
--
-- /See:/ 'newSpekeKeyProvider' smart constructor.
data SpekeKeyProvider = SpekeKeyProvider'
  { -- | An Amazon Resource Name (ARN) of a Certificate Manager certificate that
    -- MediaPackage will use for enforcing secure end-to-end data transfer with
    -- the key provider service.
    certificateArn :: Prelude.Maybe Prelude.Text,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'url'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  SpekeKeyProvider
newSpekeKeyProvider pResourceId_ pUrl_ pRoleArn_ =
  SpekeKeyProvider'
    { certificateArn = Prelude.Nothing,
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

-- | The resource ID to include in key requests.
spekeKeyProvider_resourceId :: Lens.Lens' SpekeKeyProvider Prelude.Text
spekeKeyProvider_resourceId = Lens.lens (\SpekeKeyProvider' {resourceId} -> resourceId) (\s@SpekeKeyProvider' {} a -> s {resourceId = a} :: SpekeKeyProvider)

-- | The system IDs to include in key requests.
spekeKeyProvider_systemIds :: Lens.Lens' SpekeKeyProvider [Prelude.Text]
spekeKeyProvider_systemIds = Lens.lens (\SpekeKeyProvider' {systemIds} -> systemIds) (\s@SpekeKeyProvider' {} a -> s {systemIds = a} :: SpekeKeyProvider) Prelude.. Prelude._Coerce

-- | The URL of the external key provider service.
spekeKeyProvider_url :: Lens.Lens' SpekeKeyProvider Prelude.Text
spekeKeyProvider_url = Lens.lens (\SpekeKeyProvider' {url} -> url) (\s@SpekeKeyProvider' {} a -> s {url = a} :: SpekeKeyProvider)

-- | An Amazon Resource Name (ARN) of an IAM role that AWS Elemental
-- MediaPackage will assume when accessing the key provider service.
spekeKeyProvider_roleArn :: Lens.Lens' SpekeKeyProvider Prelude.Text
spekeKeyProvider_roleArn = Lens.lens (\SpekeKeyProvider' {roleArn} -> roleArn) (\s@SpekeKeyProvider' {} a -> s {roleArn = a} :: SpekeKeyProvider)

instance Prelude.FromJSON SpekeKeyProvider where
  parseJSON =
    Prelude.withObject
      "SpekeKeyProvider"
      ( \x ->
          SpekeKeyProvider'
            Prelude.<$> (x Prelude..:? "certificateArn")
            Prelude.<*> (x Prelude..: "resourceId")
            Prelude.<*> ( x Prelude..:? "systemIds"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..: "url")
            Prelude.<*> (x Prelude..: "roleArn")
      )

instance Prelude.Hashable SpekeKeyProvider

instance Prelude.NFData SpekeKeyProvider

instance Prelude.ToJSON SpekeKeyProvider where
  toJSON SpekeKeyProvider' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("certificateArn" Prelude..=)
              Prelude.<$> certificateArn,
            Prelude.Just ("resourceId" Prelude..= resourceId),
            Prelude.Just ("systemIds" Prelude..= systemIds),
            Prelude.Just ("url" Prelude..= url),
            Prelude.Just ("roleArn" Prelude..= roleArn)
          ]
      )
