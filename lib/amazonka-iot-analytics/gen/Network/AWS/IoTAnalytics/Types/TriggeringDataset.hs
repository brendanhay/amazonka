{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.TriggeringDataset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.TriggeringDataset
  ( TriggeringDataset (..)
  -- * Smart constructor
  , mkTriggeringDataset
  -- * Lenses
  , tdName
  ) where

import qualified Network.AWS.IoTAnalytics.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the dataset whose content generation triggers the new dataset content generation.
--
-- /See:/ 'mkTriggeringDataset' smart constructor.
newtype TriggeringDataset = TriggeringDataset'
  { name :: Types.Name
    -- ^ The name of the dataset whose content generation triggers the new dataset content generation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TriggeringDataset' value with any optional fields omitted.
mkTriggeringDataset
    :: Types.Name -- ^ 'name'
    -> TriggeringDataset
mkTriggeringDataset name = TriggeringDataset'{name}

-- | The name of the dataset whose content generation triggers the new dataset content generation.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdName :: Lens.Lens' TriggeringDataset Types.Name
tdName = Lens.field @"name"
{-# INLINEABLE tdName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON TriggeringDataset where
        toJSON TriggeringDataset{..}
          = Core.object (Core.catMaybes [Core.Just ("name" Core..= name)])

instance Core.FromJSON TriggeringDataset where
        parseJSON
          = Core.withObject "TriggeringDataset" Core.$
              \ x -> TriggeringDataset' Core.<$> (x Core..: "name")
