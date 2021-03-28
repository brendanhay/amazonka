{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatastoreStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.DatastoreStatistics
  ( DatastoreStatistics (..)
  -- * Smart constructor
  , mkDatastoreStatistics
  -- * Lenses
  , dsSize
  ) where

import qualified Network.AWS.IoTAnalytics.Types.EstimatedResourceSize as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Statistical information about the data store.
--
-- /See:/ 'mkDatastoreStatistics' smart constructor.
newtype DatastoreStatistics = DatastoreStatistics'
  { size :: Core.Maybe Types.EstimatedResourceSize
    -- ^ The estimated size of the data store.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype Core.NFData

-- | Creates a 'DatastoreStatistics' value with any optional fields omitted.
mkDatastoreStatistics
    :: DatastoreStatistics
mkDatastoreStatistics = DatastoreStatistics'{size = Core.Nothing}

-- | The estimated size of the data store.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSize :: Lens.Lens' DatastoreStatistics (Core.Maybe Types.EstimatedResourceSize)
dsSize = Lens.field @"size"
{-# INLINEABLE dsSize #-}
{-# DEPRECATED size "Use generic-lens or generic-optics with 'size' instead"  #-}

instance Core.FromJSON DatastoreStatistics where
        parseJSON
          = Core.withObject "DatastoreStatistics" Core.$
              \ x -> DatastoreStatistics' Core.<$> (x Core..:? "size")
