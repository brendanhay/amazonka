{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetContentDeliveryRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.DatasetContentDeliveryRule
  ( DatasetContentDeliveryRule (..)
  -- * Smart constructor
  , mkDatasetContentDeliveryRule
  -- * Lenses
  , dcdrDestination
  , dcdrEntryName
  ) where

import qualified Network.AWS.IoTAnalytics.Types.DatasetContentDeliveryDestination as Types
import qualified Network.AWS.IoTAnalytics.Types.EntryName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | When dataset contents are created, they are delivered to destination specified here.
--
-- /See:/ 'mkDatasetContentDeliveryRule' smart constructor.
data DatasetContentDeliveryRule = DatasetContentDeliveryRule'
  { destination :: Types.DatasetContentDeliveryDestination
    -- ^ The destination to which dataset contents are delivered.
  , entryName :: Core.Maybe Types.EntryName
    -- ^ The name of the dataset content delivery rules entry.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DatasetContentDeliveryRule' value with any optional fields omitted.
mkDatasetContentDeliveryRule
    :: Types.DatasetContentDeliveryDestination -- ^ 'destination'
    -> DatasetContentDeliveryRule
mkDatasetContentDeliveryRule destination
  = DatasetContentDeliveryRule'{destination,
                                entryName = Core.Nothing}

-- | The destination to which dataset contents are delivered.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcdrDestination :: Lens.Lens' DatasetContentDeliveryRule Types.DatasetContentDeliveryDestination
dcdrDestination = Lens.field @"destination"
{-# INLINEABLE dcdrDestination #-}
{-# DEPRECATED destination "Use generic-lens or generic-optics with 'destination' instead"  #-}

-- | The name of the dataset content delivery rules entry.
--
-- /Note:/ Consider using 'entryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcdrEntryName :: Lens.Lens' DatasetContentDeliveryRule (Core.Maybe Types.EntryName)
dcdrEntryName = Lens.field @"entryName"
{-# INLINEABLE dcdrEntryName #-}
{-# DEPRECATED entryName "Use generic-lens or generic-optics with 'entryName' instead"  #-}

instance Core.FromJSON DatasetContentDeliveryRule where
        toJSON DatasetContentDeliveryRule{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("destination" Core..= destination),
                  ("entryName" Core..=) Core.<$> entryName])

instance Core.FromJSON DatasetContentDeliveryRule where
        parseJSON
          = Core.withObject "DatasetContentDeliveryRule" Core.$
              \ x ->
                DatasetContentDeliveryRule' Core.<$>
                  (x Core..: "destination") Core.<*> x Core..:? "entryName"
