{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.SnapshotOptionsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.SnapshotOptionsStatus
  ( SnapshotOptionsStatus (..)
  -- * Smart constructor
  , mkSnapshotOptionsStatus
  -- * Lenses
  , sosOptions
  , sosStatus
  ) where

import qualified Network.AWS.ElasticSearch.Types.OptionStatus as Types
import qualified Network.AWS.ElasticSearch.Types.SnapshotOptions as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Status of a daily automated snapshot.
--
-- /See:/ 'mkSnapshotOptionsStatus' smart constructor.
data SnapshotOptionsStatus = SnapshotOptionsStatus'
  { options :: Types.SnapshotOptions
    -- ^ Specifies the daily snapshot options specified for the Elasticsearch domain.
  , status :: Types.OptionStatus
    -- ^ Specifies the status of a daily automated snapshot.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'SnapshotOptionsStatus' value with any optional fields omitted.
mkSnapshotOptionsStatus
    :: Types.SnapshotOptions -- ^ 'options'
    -> Types.OptionStatus -- ^ 'status'
    -> SnapshotOptionsStatus
mkSnapshotOptionsStatus options status
  = SnapshotOptionsStatus'{options, status}

-- | Specifies the daily snapshot options specified for the Elasticsearch domain.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sosOptions :: Lens.Lens' SnapshotOptionsStatus Types.SnapshotOptions
sosOptions = Lens.field @"options"
{-# INLINEABLE sosOptions #-}
{-# DEPRECATED options "Use generic-lens or generic-optics with 'options' instead"  #-}

-- | Specifies the status of a daily automated snapshot.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sosStatus :: Lens.Lens' SnapshotOptionsStatus Types.OptionStatus
sosStatus = Lens.field @"status"
{-# INLINEABLE sosStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON SnapshotOptionsStatus where
        parseJSON
          = Core.withObject "SnapshotOptionsStatus" Core.$
              \ x ->
                SnapshotOptionsStatus' Core.<$>
                  (x Core..: "Options") Core.<*> x Core..: "Status"
