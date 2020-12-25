{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.SnapshotOptionsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.SnapshotOptionsStatus
  ( SnapshotOptionsStatus (..),

    -- * Smart constructor
    mkSnapshotOptionsStatus,

    -- * Lenses
    sosOptions,
    sosStatus,
  )
where

import qualified Network.AWS.ElasticSearch.Types.OptionStatus as Types
import qualified Network.AWS.ElasticSearch.Types.SnapshotOptions as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Status of a daily automated snapshot.
--
-- /See:/ 'mkSnapshotOptionsStatus' smart constructor.
data SnapshotOptionsStatus = SnapshotOptionsStatus'
  { -- | Specifies the daily snapshot options specified for the Elasticsearch domain.
    options :: Types.SnapshotOptions,
    -- | Specifies the status of a daily automated snapshot.
    status :: Types.OptionStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'SnapshotOptionsStatus' value with any optional fields omitted.
mkSnapshotOptionsStatus ::
  -- | 'options'
  Types.SnapshotOptions ->
  -- | 'status'
  Types.OptionStatus ->
  SnapshotOptionsStatus
mkSnapshotOptionsStatus options status =
  SnapshotOptionsStatus' {options, status}

-- | Specifies the daily snapshot options specified for the Elasticsearch domain.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sosOptions :: Lens.Lens' SnapshotOptionsStatus Types.SnapshotOptions
sosOptions = Lens.field @"options"
{-# DEPRECATED sosOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | Specifies the status of a daily automated snapshot.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sosStatus :: Lens.Lens' SnapshotOptionsStatus Types.OptionStatus
sosStatus = Lens.field @"status"
{-# DEPRECATED sosStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON SnapshotOptionsStatus where
  parseJSON =
    Core.withObject "SnapshotOptionsStatus" Core.$
      \x ->
        SnapshotOptionsStatus'
          Core.<$> (x Core..: "Options") Core.<*> (x Core..: "Status")
