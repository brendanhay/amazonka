{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CollectionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.CollectionConfiguration
  ( CollectionConfiguration (..)
  -- * Smart constructor
  , mkCollectionConfiguration
  -- * Lenses
  , ccCollectionName
  , ccCollectionParameters
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.CollectionName as Types
import qualified Network.AWS.SageMaker.Types.ConfigKey as Types
import qualified Network.AWS.SageMaker.Types.ConfigValue as Types

-- | Configuration information for tensor collections.
--
-- /See:/ 'mkCollectionConfiguration' smart constructor.
data CollectionConfiguration = CollectionConfiguration'
  { collectionName :: Core.Maybe Types.CollectionName
    -- ^ The name of the tensor collection. The name must be unique relative to other rule configuration names.
  , collectionParameters :: Core.Maybe (Core.HashMap Types.ConfigKey Types.ConfigValue)
    -- ^ Parameter values for the tensor collection. The allowed parameters are @"name"@ , @"include_regex"@ , @"reduction_config"@ , @"save_config"@ , @"tensor_names"@ , and @"save_histogram"@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CollectionConfiguration' value with any optional fields omitted.
mkCollectionConfiguration
    :: CollectionConfiguration
mkCollectionConfiguration
  = CollectionConfiguration'{collectionName = Core.Nothing,
                             collectionParameters = Core.Nothing}

-- | The name of the tensor collection. The name must be unique relative to other rule configuration names.
--
-- /Note:/ Consider using 'collectionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCollectionName :: Lens.Lens' CollectionConfiguration (Core.Maybe Types.CollectionName)
ccCollectionName = Lens.field @"collectionName"
{-# INLINEABLE ccCollectionName #-}
{-# DEPRECATED collectionName "Use generic-lens or generic-optics with 'collectionName' instead"  #-}

-- | Parameter values for the tensor collection. The allowed parameters are @"name"@ , @"include_regex"@ , @"reduction_config"@ , @"save_config"@ , @"tensor_names"@ , and @"save_histogram"@ .
--
-- /Note:/ Consider using 'collectionParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCollectionParameters :: Lens.Lens' CollectionConfiguration (Core.Maybe (Core.HashMap Types.ConfigKey Types.ConfigValue))
ccCollectionParameters = Lens.field @"collectionParameters"
{-# INLINEABLE ccCollectionParameters #-}
{-# DEPRECATED collectionParameters "Use generic-lens or generic-optics with 'collectionParameters' instead"  #-}

instance Core.FromJSON CollectionConfiguration where
        toJSON CollectionConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [("CollectionName" Core..=) Core.<$> collectionName,
                  ("CollectionParameters" Core..=) Core.<$> collectionParameters])

instance Core.FromJSON CollectionConfiguration where
        parseJSON
          = Core.withObject "CollectionConfiguration" Core.$
              \ x ->
                CollectionConfiguration' Core.<$>
                  (x Core..:? "CollectionName") Core.<*>
                    x Core..:? "CollectionParameters"
