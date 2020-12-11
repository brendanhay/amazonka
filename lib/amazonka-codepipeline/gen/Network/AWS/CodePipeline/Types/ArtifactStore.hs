-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ArtifactStore
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ArtifactStore
  ( ArtifactStore (..),

    -- * Smart constructor
    mkArtifactStore,

    -- * Lenses
    asEncryptionKey,
    asType,
    asLocation,
  )
where

import Network.AWS.CodePipeline.Types.ArtifactStoreType
import Network.AWS.CodePipeline.Types.EncryptionKey
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The S3 bucket where artifacts for the pipeline are stored.
--
-- /See:/ 'mkArtifactStore' smart constructor.
data ArtifactStore = ArtifactStore'
  { encryptionKey ::
      Lude.Maybe EncryptionKey,
    type' :: ArtifactStoreType,
    location :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ArtifactStore' with the minimum fields required to make a request.
--
-- * 'encryptionKey' - The encryption key used to encrypt the data in the artifact store, such as an AWS Key Management Service (AWS KMS) key. If this is undefined, the default key for Amazon S3 is used.
-- * 'location' - The S3 bucket used for storing the artifacts for a pipeline. You can specify the name of an S3 bucket but not a folder in the bucket. A folder to contain the pipeline artifacts is created for you based on the name of the pipeline. You can use any S3 bucket in the same AWS Region as the pipeline to store your pipeline artifacts.
-- * 'type'' - The type of the artifact store, such as S3.
mkArtifactStore ::
  -- | 'type''
  ArtifactStoreType ->
  -- | 'location'
  Lude.Text ->
  ArtifactStore
mkArtifactStore pType_ pLocation_ =
  ArtifactStore'
    { encryptionKey = Lude.Nothing,
      type' = pType_,
      location = pLocation_
    }

-- | The encryption key used to encrypt the data in the artifact store, such as an AWS Key Management Service (AWS KMS) key. If this is undefined, the default key for Amazon S3 is used.
--
-- /Note:/ Consider using 'encryptionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asEncryptionKey :: Lens.Lens' ArtifactStore (Lude.Maybe EncryptionKey)
asEncryptionKey = Lens.lens (encryptionKey :: ArtifactStore -> Lude.Maybe EncryptionKey) (\s a -> s {encryptionKey = a} :: ArtifactStore)
{-# DEPRECATED asEncryptionKey "Use generic-lens or generic-optics with 'encryptionKey' instead." #-}

-- | The type of the artifact store, such as S3.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asType :: Lens.Lens' ArtifactStore ArtifactStoreType
asType = Lens.lens (type' :: ArtifactStore -> ArtifactStoreType) (\s a -> s {type' = a} :: ArtifactStore)
{-# DEPRECATED asType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The S3 bucket used for storing the artifacts for a pipeline. You can specify the name of an S3 bucket but not a folder in the bucket. A folder to contain the pipeline artifacts is created for you based on the name of the pipeline. You can use any S3 bucket in the same AWS Region as the pipeline to store your pipeline artifacts.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asLocation :: Lens.Lens' ArtifactStore Lude.Text
asLocation = Lens.lens (location :: ArtifactStore -> Lude.Text) (\s a -> s {location = a} :: ArtifactStore)
{-# DEPRECATED asLocation "Use generic-lens or generic-optics with 'location' instead." #-}

instance Lude.FromJSON ArtifactStore where
  parseJSON =
    Lude.withObject
      "ArtifactStore"
      ( \x ->
          ArtifactStore'
            Lude.<$> (x Lude..:? "encryptionKey")
            Lude.<*> (x Lude..: "type")
            Lude.<*> (x Lude..: "location")
      )

instance Lude.ToJSON ArtifactStore where
  toJSON ArtifactStore' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("encryptionKey" Lude..=) Lude.<$> encryptionKey,
            Lude.Just ("type" Lude..= type'),
            Lude.Just ("location" Lude..= location)
          ]
      )
