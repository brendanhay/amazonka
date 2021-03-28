{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.SnapshotOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.SnapshotOptions
  ( SnapshotOptions (..)
  -- * Smart constructor
  , mkSnapshotOptions
  -- * Lenses
  , soAutomatedSnapshotStartHour
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the time, in UTC format, when the service takes a daily automated snapshot of the specified Elasticsearch domain. Default value is @0@ hours.
--
-- /See:/ 'mkSnapshotOptions' smart constructor.
newtype SnapshotOptions = SnapshotOptions'
  { automatedSnapshotStartHour :: Core.Maybe Core.Int
    -- ^ Specifies the time, in UTC format, when the service takes a daily automated snapshot of the specified Elasticsearch domain. Default value is @0@ hours.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SnapshotOptions' value with any optional fields omitted.
mkSnapshotOptions
    :: SnapshotOptions
mkSnapshotOptions
  = SnapshotOptions'{automatedSnapshotStartHour = Core.Nothing}

-- | Specifies the time, in UTC format, when the service takes a daily automated snapshot of the specified Elasticsearch domain. Default value is @0@ hours.
--
-- /Note:/ Consider using 'automatedSnapshotStartHour' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
soAutomatedSnapshotStartHour :: Lens.Lens' SnapshotOptions (Core.Maybe Core.Int)
soAutomatedSnapshotStartHour = Lens.field @"automatedSnapshotStartHour"
{-# INLINEABLE soAutomatedSnapshotStartHour #-}
{-# DEPRECATED automatedSnapshotStartHour "Use generic-lens or generic-optics with 'automatedSnapshotStartHour' instead"  #-}

instance Core.FromJSON SnapshotOptions where
        toJSON SnapshotOptions{..}
          = Core.object
              (Core.catMaybes
                 [("AutomatedSnapshotStartHour" Core..=) Core.<$>
                    automatedSnapshotStartHour])

instance Core.FromJSON SnapshotOptions where
        parseJSON
          = Core.withObject "SnapshotOptions" Core.$
              \ x ->
                SnapshotOptions' Core.<$> (x Core..:? "AutomatedSnapshotStartHour")
