{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    asType,
    asLocation,
    asEncryptionKey,
  )
where

import qualified Network.AWS.CodePipeline.Types.ArtifactStoreLocation as Types
import qualified Network.AWS.CodePipeline.Types.ArtifactStoreType as Types
import qualified Network.AWS.CodePipeline.Types.EncryptionKey as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The S3 bucket where artifacts for the pipeline are stored.
--
-- /See:/ 'mkArtifactStore' smart constructor.
data ArtifactStore = ArtifactStore'
  { -- | The type of the artifact store, such as S3.
    type' :: Types.ArtifactStoreType,
    -- | The S3 bucket used for storing the artifacts for a pipeline. You can specify the name of an S3 bucket but not a folder in the bucket. A folder to contain the pipeline artifacts is created for you based on the name of the pipeline. You can use any S3 bucket in the same AWS Region as the pipeline to store your pipeline artifacts.
    location :: Types.ArtifactStoreLocation,
    -- | The encryption key used to encrypt the data in the artifact store, such as an AWS Key Management Service (AWS KMS) key. If this is undefined, the default key for Amazon S3 is used.
    encryptionKey :: Core.Maybe Types.EncryptionKey
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ArtifactStore' value with any optional fields omitted.
mkArtifactStore ::
  -- | 'type\''
  Types.ArtifactStoreType ->
  -- | 'location'
  Types.ArtifactStoreLocation ->
  ArtifactStore
mkArtifactStore type' location =
  ArtifactStore' {type', location, encryptionKey = Core.Nothing}

-- | The type of the artifact store, such as S3.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asType :: Lens.Lens' ArtifactStore Types.ArtifactStoreType
asType = Lens.field @"type'"
{-# DEPRECATED asType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The S3 bucket used for storing the artifacts for a pipeline. You can specify the name of an S3 bucket but not a folder in the bucket. A folder to contain the pipeline artifacts is created for you based on the name of the pipeline. You can use any S3 bucket in the same AWS Region as the pipeline to store your pipeline artifacts.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asLocation :: Lens.Lens' ArtifactStore Types.ArtifactStoreLocation
asLocation = Lens.field @"location"
{-# DEPRECATED asLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The encryption key used to encrypt the data in the artifact store, such as an AWS Key Management Service (AWS KMS) key. If this is undefined, the default key for Amazon S3 is used.
--
-- /Note:/ Consider using 'encryptionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asEncryptionKey :: Lens.Lens' ArtifactStore (Core.Maybe Types.EncryptionKey)
asEncryptionKey = Lens.field @"encryptionKey"
{-# DEPRECATED asEncryptionKey "Use generic-lens or generic-optics with 'encryptionKey' instead." #-}

instance Core.FromJSON ArtifactStore where
  toJSON ArtifactStore {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("type" Core..= type'),
            Core.Just ("location" Core..= location),
            ("encryptionKey" Core..=) Core.<$> encryptionKey
          ]
      )

instance Core.FromJSON ArtifactStore where
  parseJSON =
    Core.withObject "ArtifactStore" Core.$
      \x ->
        ArtifactStore'
          Core.<$> (x Core..: "type")
          Core.<*> (x Core..: "location")
          Core.<*> (x Core..:? "encryptionKey")
