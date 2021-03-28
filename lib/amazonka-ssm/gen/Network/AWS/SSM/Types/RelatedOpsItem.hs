{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.RelatedOpsItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.RelatedOpsItem
  ( RelatedOpsItem (..)
  -- * Smart constructor
  , mkRelatedOpsItem
  -- * Lenses
  , roiOpsItemId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An OpsItems that shares something in common with the current OpsItem. For example, related OpsItems can include OpsItems with similar error messages, impacted resources, or statuses for the impacted resource.
--
-- /See:/ 'mkRelatedOpsItem' smart constructor.
newtype RelatedOpsItem = RelatedOpsItem'
  { opsItemId :: Core.Text
    -- ^ The ID of an OpsItem related to the current OpsItem.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RelatedOpsItem' value with any optional fields omitted.
mkRelatedOpsItem
    :: Core.Text -- ^ 'opsItemId'
    -> RelatedOpsItem
mkRelatedOpsItem opsItemId = RelatedOpsItem'{opsItemId}

-- | The ID of an OpsItem related to the current OpsItem.
--
-- /Note:/ Consider using 'opsItemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
roiOpsItemId :: Lens.Lens' RelatedOpsItem Core.Text
roiOpsItemId = Lens.field @"opsItemId"
{-# INLINEABLE roiOpsItemId #-}
{-# DEPRECATED opsItemId "Use generic-lens or generic-optics with 'opsItemId' instead"  #-}

instance Core.FromJSON RelatedOpsItem where
        toJSON RelatedOpsItem{..}
          = Core.object
              (Core.catMaybes [Core.Just ("OpsItemId" Core..= opsItemId)])

instance Core.FromJSON RelatedOpsItem where
        parseJSON
          = Core.withObject "RelatedOpsItem" Core.$
              \ x -> RelatedOpsItem' Core.<$> (x Core..: "OpsItemId")
