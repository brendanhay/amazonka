{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.DebugHookConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DebugHookConfig
  ( DebugHookConfig (..),

    -- * Smart constructor
    mkDebugHookConfig,

    -- * Lenses
    dhcLocalPath,
    dhcCollectionConfigurations,
    dhcHookParameters,
    dhcS3OutputPath,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.CollectionConfiguration

-- | Configuration information for the debug hook parameters, collection configuration, and storage paths.
--
-- /See:/ 'mkDebugHookConfig' smart constructor.
data DebugHookConfig = DebugHookConfig'
  { localPath ::
      Lude.Maybe Lude.Text,
    collectionConfigurations ::
      Lude.Maybe [CollectionConfiguration],
    hookParameters ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    s3OutputPath :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DebugHookConfig' with the minimum fields required to make a request.
--
-- * 'collectionConfigurations' - Configuration information for tensor collections.
-- * 'hookParameters' - Configuration information for the debug hook parameters.
-- * 'localPath' - Path to local storage location for tensors. Defaults to @/opt/ml/output/tensors/@ .
-- * 's3OutputPath' - Path to Amazon S3 storage location for tensors.
mkDebugHookConfig ::
  -- | 's3OutputPath'
  Lude.Text ->
  DebugHookConfig
mkDebugHookConfig pS3OutputPath_ =
  DebugHookConfig'
    { localPath = Lude.Nothing,
      collectionConfigurations = Lude.Nothing,
      hookParameters = Lude.Nothing,
      s3OutputPath = pS3OutputPath_
    }

-- | Path to local storage location for tensors. Defaults to @/opt/ml/output/tensors/@ .
--
-- /Note:/ Consider using 'localPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcLocalPath :: Lens.Lens' DebugHookConfig (Lude.Maybe Lude.Text)
dhcLocalPath = Lens.lens (localPath :: DebugHookConfig -> Lude.Maybe Lude.Text) (\s a -> s {localPath = a} :: DebugHookConfig)
{-# DEPRECATED dhcLocalPath "Use generic-lens or generic-optics with 'localPath' instead." #-}

-- | Configuration information for tensor collections.
--
-- /Note:/ Consider using 'collectionConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcCollectionConfigurations :: Lens.Lens' DebugHookConfig (Lude.Maybe [CollectionConfiguration])
dhcCollectionConfigurations = Lens.lens (collectionConfigurations :: DebugHookConfig -> Lude.Maybe [CollectionConfiguration]) (\s a -> s {collectionConfigurations = a} :: DebugHookConfig)
{-# DEPRECATED dhcCollectionConfigurations "Use generic-lens or generic-optics with 'collectionConfigurations' instead." #-}

-- | Configuration information for the debug hook parameters.
--
-- /Note:/ Consider using 'hookParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcHookParameters :: Lens.Lens' DebugHookConfig (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
dhcHookParameters = Lens.lens (hookParameters :: DebugHookConfig -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {hookParameters = a} :: DebugHookConfig)
{-# DEPRECATED dhcHookParameters "Use generic-lens or generic-optics with 'hookParameters' instead." #-}

-- | Path to Amazon S3 storage location for tensors.
--
-- /Note:/ Consider using 's3OutputPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcS3OutputPath :: Lens.Lens' DebugHookConfig Lude.Text
dhcS3OutputPath = Lens.lens (s3OutputPath :: DebugHookConfig -> Lude.Text) (\s a -> s {s3OutputPath = a} :: DebugHookConfig)
{-# DEPRECATED dhcS3OutputPath "Use generic-lens or generic-optics with 's3OutputPath' instead." #-}

instance Lude.FromJSON DebugHookConfig where
  parseJSON =
    Lude.withObject
      "DebugHookConfig"
      ( \x ->
          DebugHookConfig'
            Lude.<$> (x Lude..:? "LocalPath")
            Lude.<*> (x Lude..:? "CollectionConfigurations" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "HookParameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "S3OutputPath")
      )

instance Lude.ToJSON DebugHookConfig where
  toJSON DebugHookConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("LocalPath" Lude..=) Lude.<$> localPath,
            ("CollectionConfigurations" Lude..=)
              Lude.<$> collectionConfigurations,
            ("HookParameters" Lude..=) Lude.<$> hookParameters,
            Lude.Just ("S3OutputPath" Lude..= s3OutputPath)
          ]
      )
