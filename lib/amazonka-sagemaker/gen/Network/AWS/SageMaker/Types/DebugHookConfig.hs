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
    dhcS3OutputPath,
    dhcCollectionConfigurations,
    dhcHookParameters,
    dhcLocalPath,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.CollectionConfiguration as Types
import qualified Network.AWS.SageMaker.Types.ConfigKey as Types
import qualified Network.AWS.SageMaker.Types.ConfigValue as Types
import qualified Network.AWS.SageMaker.Types.LocalPath as Types
import qualified Network.AWS.SageMaker.Types.S3OutputPath as Types

-- | Configuration information for the debug hook parameters, collection configuration, and storage paths.
--
-- /See:/ 'mkDebugHookConfig' smart constructor.
data DebugHookConfig = DebugHookConfig'
  { -- | Path to Amazon S3 storage location for tensors.
    s3OutputPath :: Types.S3OutputPath,
    -- | Configuration information for tensor collections.
    collectionConfigurations :: Core.Maybe [Types.CollectionConfiguration],
    -- | Configuration information for the debug hook parameters.
    hookParameters :: Core.Maybe (Core.HashMap Types.ConfigKey Types.ConfigValue),
    -- | Path to local storage location for tensors. Defaults to @/opt/ml/output/tensors/@ .
    localPath :: Core.Maybe Types.LocalPath
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DebugHookConfig' value with any optional fields omitted.
mkDebugHookConfig ::
  -- | 's3OutputPath'
  Types.S3OutputPath ->
  DebugHookConfig
mkDebugHookConfig s3OutputPath =
  DebugHookConfig'
    { s3OutputPath,
      collectionConfigurations = Core.Nothing,
      hookParameters = Core.Nothing,
      localPath = Core.Nothing
    }

-- | Path to Amazon S3 storage location for tensors.
--
-- /Note:/ Consider using 's3OutputPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcS3OutputPath :: Lens.Lens' DebugHookConfig Types.S3OutputPath
dhcS3OutputPath = Lens.field @"s3OutputPath"
{-# DEPRECATED dhcS3OutputPath "Use generic-lens or generic-optics with 's3OutputPath' instead." #-}

-- | Configuration information for tensor collections.
--
-- /Note:/ Consider using 'collectionConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcCollectionConfigurations :: Lens.Lens' DebugHookConfig (Core.Maybe [Types.CollectionConfiguration])
dhcCollectionConfigurations = Lens.field @"collectionConfigurations"
{-# DEPRECATED dhcCollectionConfigurations "Use generic-lens or generic-optics with 'collectionConfigurations' instead." #-}

-- | Configuration information for the debug hook parameters.
--
-- /Note:/ Consider using 'hookParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcHookParameters :: Lens.Lens' DebugHookConfig (Core.Maybe (Core.HashMap Types.ConfigKey Types.ConfigValue))
dhcHookParameters = Lens.field @"hookParameters"
{-# DEPRECATED dhcHookParameters "Use generic-lens or generic-optics with 'hookParameters' instead." #-}

-- | Path to local storage location for tensors. Defaults to @/opt/ml/output/tensors/@ .
--
-- /Note:/ Consider using 'localPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcLocalPath :: Lens.Lens' DebugHookConfig (Core.Maybe Types.LocalPath)
dhcLocalPath = Lens.field @"localPath"
{-# DEPRECATED dhcLocalPath "Use generic-lens or generic-optics with 'localPath' instead." #-}

instance Core.FromJSON DebugHookConfig where
  toJSON DebugHookConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("S3OutputPath" Core..= s3OutputPath),
            ("CollectionConfigurations" Core..=)
              Core.<$> collectionConfigurations,
            ("HookParameters" Core..=) Core.<$> hookParameters,
            ("LocalPath" Core..=) Core.<$> localPath
          ]
      )

instance Core.FromJSON DebugHookConfig where
  parseJSON =
    Core.withObject "DebugHookConfig" Core.$
      \x ->
        DebugHookConfig'
          Core.<$> (x Core..: "S3OutputPath")
          Core.<*> (x Core..:? "CollectionConfigurations")
          Core.<*> (x Core..:? "HookParameters")
          Core.<*> (x Core..:? "LocalPath")
