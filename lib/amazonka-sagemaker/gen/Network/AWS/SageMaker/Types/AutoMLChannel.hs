{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLChannel
  ( AutoMLChannel (..),

    -- * Smart constructor
    mkAutoMLChannel,

    -- * Lenses
    amlcDataSource,
    amlcTargetAttributeName,
    amlcCompressionType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.AutoMLDataSource as Types
import qualified Network.AWS.SageMaker.Types.CompressionType as Types
import qualified Network.AWS.SageMaker.Types.TargetAttributeName as Types

-- | Similar to Channel. A channel is a named input source that training algorithms can consume. Refer to Channel for detailed descriptions.
--
-- /See:/ 'mkAutoMLChannel' smart constructor.
data AutoMLChannel = AutoMLChannel'
  { -- | The data source.
    dataSource :: Types.AutoMLDataSource,
    -- | The name of the target variable in supervised learning, a.k.a. 'y'.
    targetAttributeName :: Types.TargetAttributeName,
    -- | You can use Gzip or None. The default value is None.
    compressionType :: Core.Maybe Types.CompressionType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AutoMLChannel' value with any optional fields omitted.
mkAutoMLChannel ::
  -- | 'dataSource'
  Types.AutoMLDataSource ->
  -- | 'targetAttributeName'
  Types.TargetAttributeName ->
  AutoMLChannel
mkAutoMLChannel dataSource targetAttributeName =
  AutoMLChannel'
    { dataSource,
      targetAttributeName,
      compressionType = Core.Nothing
    }

-- | The data source.
--
-- /Note:/ Consider using 'dataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcDataSource :: Lens.Lens' AutoMLChannel Types.AutoMLDataSource
amlcDataSource = Lens.field @"dataSource"
{-# DEPRECATED amlcDataSource "Use generic-lens or generic-optics with 'dataSource' instead." #-}

-- | The name of the target variable in supervised learning, a.k.a. 'y'.
--
-- /Note:/ Consider using 'targetAttributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcTargetAttributeName :: Lens.Lens' AutoMLChannel Types.TargetAttributeName
amlcTargetAttributeName = Lens.field @"targetAttributeName"
{-# DEPRECATED amlcTargetAttributeName "Use generic-lens or generic-optics with 'targetAttributeName' instead." #-}

-- | You can use Gzip or None. The default value is None.
--
-- /Note:/ Consider using 'compressionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcCompressionType :: Lens.Lens' AutoMLChannel (Core.Maybe Types.CompressionType)
amlcCompressionType = Lens.field @"compressionType"
{-# DEPRECATED amlcCompressionType "Use generic-lens or generic-optics with 'compressionType' instead." #-}

instance Core.FromJSON AutoMLChannel where
  toJSON AutoMLChannel {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DataSource" Core..= dataSource),
            Core.Just ("TargetAttributeName" Core..= targetAttributeName),
            ("CompressionType" Core..=) Core.<$> compressionType
          ]
      )

instance Core.FromJSON AutoMLChannel where
  parseJSON =
    Core.withObject "AutoMLChannel" Core.$
      \x ->
        AutoMLChannel'
          Core.<$> (x Core..: "DataSource")
          Core.<*> (x Core..: "TargetAttributeName")
          Core.<*> (x Core..:? "CompressionType")
