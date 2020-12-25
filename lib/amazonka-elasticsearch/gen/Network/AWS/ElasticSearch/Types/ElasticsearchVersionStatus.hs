{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.ElasticsearchVersionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.ElasticsearchVersionStatus
  ( ElasticsearchVersionStatus (..),

    -- * Smart constructor
    mkElasticsearchVersionStatus,

    -- * Lenses
    evsOptions,
    evsStatus,
  )
where

import qualified Network.AWS.ElasticSearch.Types.ElasticsearchVersionString as Types
import qualified Network.AWS.ElasticSearch.Types.OptionStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Status of the Elasticsearch version options for the specified Elasticsearch domain.
--
-- /See:/ 'mkElasticsearchVersionStatus' smart constructor.
data ElasticsearchVersionStatus = ElasticsearchVersionStatus'
  { -- | Specifies the Elasticsearch version for the specified Elasticsearch domain.
    options :: Types.ElasticsearchVersionString,
    -- | Specifies the status of the Elasticsearch version options for the specified Elasticsearch domain.
    status :: Types.OptionStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ElasticsearchVersionStatus' value with any optional fields omitted.
mkElasticsearchVersionStatus ::
  -- | 'options'
  Types.ElasticsearchVersionString ->
  -- | 'status'
  Types.OptionStatus ->
  ElasticsearchVersionStatus
mkElasticsearchVersionStatus options status =
  ElasticsearchVersionStatus' {options, status}

-- | Specifies the Elasticsearch version for the specified Elasticsearch domain.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evsOptions :: Lens.Lens' ElasticsearchVersionStatus Types.ElasticsearchVersionString
evsOptions = Lens.field @"options"
{-# DEPRECATED evsOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | Specifies the status of the Elasticsearch version options for the specified Elasticsearch domain.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evsStatus :: Lens.Lens' ElasticsearchVersionStatus Types.OptionStatus
evsStatus = Lens.field @"status"
{-# DEPRECATED evsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON ElasticsearchVersionStatus where
  parseJSON =
    Core.withObject "ElasticsearchVersionStatus" Core.$
      \x ->
        ElasticsearchVersionStatus'
          Core.<$> (x Core..: "Options") Core.<*> (x Core..: "Status")
