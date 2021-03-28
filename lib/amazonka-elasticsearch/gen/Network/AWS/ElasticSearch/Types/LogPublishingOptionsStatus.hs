{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.LogPublishingOptionsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.LogPublishingOptionsStatus
  ( LogPublishingOptionsStatus (..)
  -- * Smart constructor
  , mkLogPublishingOptionsStatus
  -- * Lenses
  , lposOptions
  , lposStatus
  ) where

import qualified Network.AWS.ElasticSearch.Types.LogPublishingOption as Types
import qualified Network.AWS.ElasticSearch.Types.LogType as Types
import qualified Network.AWS.ElasticSearch.Types.OptionStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The configured log publishing options for the domain and their current status.
--
-- /See:/ 'mkLogPublishingOptionsStatus' smart constructor.
data LogPublishingOptionsStatus = LogPublishingOptionsStatus'
  { options :: Core.Maybe (Core.HashMap Types.LogType Types.LogPublishingOption)
    -- ^ The log publishing options configured for the Elasticsearch domain.
  , status :: Core.Maybe Types.OptionStatus
    -- ^ The status of the log publishing options for the Elasticsearch domain. See @OptionStatus@ for the status information that's included. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'LogPublishingOptionsStatus' value with any optional fields omitted.
mkLogPublishingOptionsStatus
    :: LogPublishingOptionsStatus
mkLogPublishingOptionsStatus
  = LogPublishingOptionsStatus'{options = Core.Nothing,
                                status = Core.Nothing}

-- | The log publishing options configured for the Elasticsearch domain.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lposOptions :: Lens.Lens' LogPublishingOptionsStatus (Core.Maybe (Core.HashMap Types.LogType Types.LogPublishingOption))
lposOptions = Lens.field @"options"
{-# INLINEABLE lposOptions #-}
{-# DEPRECATED options "Use generic-lens or generic-optics with 'options' instead"  #-}

-- | The status of the log publishing options for the Elasticsearch domain. See @OptionStatus@ for the status information that's included. 
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lposStatus :: Lens.Lens' LogPublishingOptionsStatus (Core.Maybe Types.OptionStatus)
lposStatus = Lens.field @"status"
{-# INLINEABLE lposStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON LogPublishingOptionsStatus where
        parseJSON
          = Core.withObject "LogPublishingOptionsStatus" Core.$
              \ x ->
                LogPublishingOptionsStatus' Core.<$>
                  (x Core..:? "Options") Core.<*> x Core..:? "Status"
