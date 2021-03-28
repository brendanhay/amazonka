{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.AutoMLChannel
  ( AutoMLChannel (..)
  -- * Smart constructor
  , mkAutoMLChannel
  -- * Lenses
  , amlcDataSource
  , amlcTargetAttributeName
  , amlcCompressionType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.AutoMLDataSource as Types
import qualified Network.AWS.SageMaker.Types.CompressionType as Types
import qualified Network.AWS.SageMaker.Types.TargetAttributeName as Types

-- | Similar to Channel. A channel is a named input source that training algorithms can consume. Refer to Channel for detailed descriptions.
--
-- /See:/ 'mkAutoMLChannel' smart constructor.
data AutoMLChannel = AutoMLChannel'
  { dataSource :: Types.AutoMLDataSource
    -- ^ The data source.
  , targetAttributeName :: Types.TargetAttributeName
    -- ^ The name of the target variable in supervised learning, a.k.a. 'y'.
  , compressionType :: Core.Maybe Types.CompressionType
    -- ^ You can use Gzip or None. The default value is None.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AutoMLChannel' value with any optional fields omitted.
mkAutoMLChannel
    :: Types.AutoMLDataSource -- ^ 'dataSource'
    -> Types.TargetAttributeName -- ^ 'targetAttributeName'
    -> AutoMLChannel
mkAutoMLChannel dataSource targetAttributeName
  = AutoMLChannel'{dataSource, targetAttributeName,
                   compressionType = Core.Nothing}

-- | The data source.
--
-- /Note:/ Consider using 'dataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcDataSource :: Lens.Lens' AutoMLChannel Types.AutoMLDataSource
amlcDataSource = Lens.field @"dataSource"
{-# INLINEABLE amlcDataSource #-}
{-# DEPRECATED dataSource "Use generic-lens or generic-optics with 'dataSource' instead"  #-}

-- | The name of the target variable in supervised learning, a.k.a. 'y'.
--
-- /Note:/ Consider using 'targetAttributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcTargetAttributeName :: Lens.Lens' AutoMLChannel Types.TargetAttributeName
amlcTargetAttributeName = Lens.field @"targetAttributeName"
{-# INLINEABLE amlcTargetAttributeName #-}
{-# DEPRECATED targetAttributeName "Use generic-lens or generic-optics with 'targetAttributeName' instead"  #-}

-- | You can use Gzip or None. The default value is None.
--
-- /Note:/ Consider using 'compressionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcCompressionType :: Lens.Lens' AutoMLChannel (Core.Maybe Types.CompressionType)
amlcCompressionType = Lens.field @"compressionType"
{-# INLINEABLE amlcCompressionType #-}
{-# DEPRECATED compressionType "Use generic-lens or generic-optics with 'compressionType' instead"  #-}

instance Core.FromJSON AutoMLChannel where
        toJSON AutoMLChannel{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DataSource" Core..= dataSource),
                  Core.Just ("TargetAttributeName" Core..= targetAttributeName),
                  ("CompressionType" Core..=) Core.<$> compressionType])

instance Core.FromJSON AutoMLChannel where
        parseJSON
          = Core.withObject "AutoMLChannel" Core.$
              \ x ->
                AutoMLChannel' Core.<$>
                  (x Core..: "DataSource") Core.<*> x Core..: "TargetAttributeName"
                    Core.<*> x Core..:? "CompressionType"
