{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.AdvancedOptionsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.AdvancedOptionsStatus
  ( AdvancedOptionsStatus (..)
  -- * Smart constructor
  , mkAdvancedOptionsStatus
  -- * Lenses
  , aosOptions
  , aosStatus
  ) where

import qualified Network.AWS.ElasticSearch.Types.OptionStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Status of the advanced options for the specified Elasticsearch domain. Currently, the following advanced options are available:
--
--
--     * Option to allow references to indices in an HTTP request body. Must be @false@ when configuring access to individual sub-resources. By default, the value is @true@ . See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuration Advanced Options> for more information.
--
--     * Option to specify the percentage of heap space that is allocated to field data. By default, this setting is unbounded.
--
-- For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuring Advanced Options> .
--
-- /See:/ 'mkAdvancedOptionsStatus' smart constructor.
data AdvancedOptionsStatus = AdvancedOptionsStatus'
  { options :: Core.HashMap Core.Text Core.Text
    -- ^ Specifies the status of advanced options for the specified Elasticsearch domain.
  , status :: Types.OptionStatus
    -- ^ Specifies the status of @OptionStatus@ for advanced options for the specified Elasticsearch domain.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AdvancedOptionsStatus' value with any optional fields omitted.
mkAdvancedOptionsStatus
    :: Types.OptionStatus -- ^ 'status'
    -> AdvancedOptionsStatus
mkAdvancedOptionsStatus status
  = AdvancedOptionsStatus'{options = Core.mempty, status}

-- | Specifies the status of advanced options for the specified Elasticsearch domain.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aosOptions :: Lens.Lens' AdvancedOptionsStatus (Core.HashMap Core.Text Core.Text)
aosOptions = Lens.field @"options"
{-# INLINEABLE aosOptions #-}
{-# DEPRECATED options "Use generic-lens or generic-optics with 'options' instead"  #-}

-- | Specifies the status of @OptionStatus@ for advanced options for the specified Elasticsearch domain.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aosStatus :: Lens.Lens' AdvancedOptionsStatus Types.OptionStatus
aosStatus = Lens.field @"status"
{-# INLINEABLE aosStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON AdvancedOptionsStatus where
        parseJSON
          = Core.withObject "AdvancedOptionsStatus" Core.$
              \ x ->
                AdvancedOptionsStatus' Core.<$>
                  (x Core..:? "Options" Core..!= Core.mempty) Core.<*>
                    x Core..: "Status"
