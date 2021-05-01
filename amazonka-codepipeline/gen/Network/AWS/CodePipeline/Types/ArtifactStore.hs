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
-- Module      : Network.AWS.CodePipeline.Types.ArtifactStore
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ArtifactStore where

import Network.AWS.CodePipeline.Types.ArtifactStoreType
import Network.AWS.CodePipeline.Types.EncryptionKey
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The S3 bucket where artifacts for the pipeline are stored.
--
-- You must include either @artifactStore@ or @artifactStores@ in your
-- pipeline, but you cannot use both. If you create a cross-region action
-- in your pipeline, you must use @artifactStores@.
--
-- /See:/ 'newArtifactStore' smart constructor.
data ArtifactStore = ArtifactStore'
  { -- | The encryption key used to encrypt the data in the artifact store, such
    -- as an AWS Key Management Service (AWS KMS) key. If this is undefined,
    -- the default key for Amazon S3 is used.
    encryptionKey :: Prelude.Maybe EncryptionKey,
    -- | The type of the artifact store, such as S3.
    type' :: ArtifactStoreType,
    -- | The S3 bucket used for storing the artifacts for a pipeline. You can
    -- specify the name of an S3 bucket but not a folder in the bucket. A
    -- folder to contain the pipeline artifacts is created for you based on the
    -- name of the pipeline. You can use any S3 bucket in the same AWS Region
    -- as the pipeline to store your pipeline artifacts.
    location :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ArtifactStore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionKey', 'artifactStore_encryptionKey' - The encryption key used to encrypt the data in the artifact store, such
-- as an AWS Key Management Service (AWS KMS) key. If this is undefined,
-- the default key for Amazon S3 is used.
--
-- 'type'', 'artifactStore_type' - The type of the artifact store, such as S3.
--
-- 'location', 'artifactStore_location' - The S3 bucket used for storing the artifacts for a pipeline. You can
-- specify the name of an S3 bucket but not a folder in the bucket. A
-- folder to contain the pipeline artifacts is created for you based on the
-- name of the pipeline. You can use any S3 bucket in the same AWS Region
-- as the pipeline to store your pipeline artifacts.
newArtifactStore ::
  -- | 'type''
  ArtifactStoreType ->
  -- | 'location'
  Prelude.Text ->
  ArtifactStore
newArtifactStore pType_ pLocation_ =
  ArtifactStore'
    { encryptionKey = Prelude.Nothing,
      type' = pType_,
      location = pLocation_
    }

-- | The encryption key used to encrypt the data in the artifact store, such
-- as an AWS Key Management Service (AWS KMS) key. If this is undefined,
-- the default key for Amazon S3 is used.
artifactStore_encryptionKey :: Lens.Lens' ArtifactStore (Prelude.Maybe EncryptionKey)
artifactStore_encryptionKey = Lens.lens (\ArtifactStore' {encryptionKey} -> encryptionKey) (\s@ArtifactStore' {} a -> s {encryptionKey = a} :: ArtifactStore)

-- | The type of the artifact store, such as S3.
artifactStore_type :: Lens.Lens' ArtifactStore ArtifactStoreType
artifactStore_type = Lens.lens (\ArtifactStore' {type'} -> type') (\s@ArtifactStore' {} a -> s {type' = a} :: ArtifactStore)

-- | The S3 bucket used for storing the artifacts for a pipeline. You can
-- specify the name of an S3 bucket but not a folder in the bucket. A
-- folder to contain the pipeline artifacts is created for you based on the
-- name of the pipeline. You can use any S3 bucket in the same AWS Region
-- as the pipeline to store your pipeline artifacts.
artifactStore_location :: Lens.Lens' ArtifactStore Prelude.Text
artifactStore_location = Lens.lens (\ArtifactStore' {location} -> location) (\s@ArtifactStore' {} a -> s {location = a} :: ArtifactStore)

instance Prelude.FromJSON ArtifactStore where
  parseJSON =
    Prelude.withObject
      "ArtifactStore"
      ( \x ->
          ArtifactStore'
            Prelude.<$> (x Prelude..:? "encryptionKey")
            Prelude.<*> (x Prelude..: "type")
            Prelude.<*> (x Prelude..: "location")
      )

instance Prelude.Hashable ArtifactStore

instance Prelude.NFData ArtifactStore

instance Prelude.ToJSON ArtifactStore where
  toJSON ArtifactStore' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("encryptionKey" Prelude..=)
              Prelude.<$> encryptionKey,
            Prelude.Just ("type" Prelude..= type'),
            Prelude.Just ("location" Prelude..= location)
          ]
      )
