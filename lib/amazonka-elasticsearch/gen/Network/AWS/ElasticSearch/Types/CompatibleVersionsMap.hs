{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.CompatibleVersionsMap
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.CompatibleVersionsMap
  ( CompatibleVersionsMap (..)
  -- * Smart constructor
  , mkCompatibleVersionsMap
  -- * Lenses
  , cvmSourceVersion
  , cvmTargetVersions
  ) where

import qualified Network.AWS.ElasticSearch.Types.ElasticsearchVersionString as Types
import qualified Network.AWS.ElasticSearch.Types.SourceVersion as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A map from an @'ElasticsearchVersion' @ to a list of compatible @'ElasticsearchVersion' @ s to which the domain can be upgraded. 
--
-- /See:/ 'mkCompatibleVersionsMap' smart constructor.
data CompatibleVersionsMap = CompatibleVersionsMap'
  { sourceVersion :: Core.Maybe Types.SourceVersion
    -- ^ The current version of Elasticsearch on which a domain is.
  , targetVersions :: Core.Maybe [Types.ElasticsearchVersionString]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CompatibleVersionsMap' value with any optional fields omitted.
mkCompatibleVersionsMap
    :: CompatibleVersionsMap
mkCompatibleVersionsMap
  = CompatibleVersionsMap'{sourceVersion = Core.Nothing,
                           targetVersions = Core.Nothing}

-- | The current version of Elasticsearch on which a domain is.
--
-- /Note:/ Consider using 'sourceVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvmSourceVersion :: Lens.Lens' CompatibleVersionsMap (Core.Maybe Types.SourceVersion)
cvmSourceVersion = Lens.field @"sourceVersion"
{-# INLINEABLE cvmSourceVersion #-}
{-# DEPRECATED sourceVersion "Use generic-lens or generic-optics with 'sourceVersion' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'targetVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvmTargetVersions :: Lens.Lens' CompatibleVersionsMap (Core.Maybe [Types.ElasticsearchVersionString])
cvmTargetVersions = Lens.field @"targetVersions"
{-# INLINEABLE cvmTargetVersions #-}
{-# DEPRECATED targetVersions "Use generic-lens or generic-optics with 'targetVersions' instead"  #-}

instance Core.FromJSON CompatibleVersionsMap where
        parseJSON
          = Core.withObject "CompatibleVersionsMap" Core.$
              \ x ->
                CompatibleVersionsMap' Core.<$>
                  (x Core..:? "SourceVersion") Core.<*> x Core..:? "TargetVersions"
