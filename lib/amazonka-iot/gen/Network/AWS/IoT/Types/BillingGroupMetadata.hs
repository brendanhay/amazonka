{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.BillingGroupMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.BillingGroupMetadata
  ( BillingGroupMetadata (..)
  -- * Smart constructor
  , mkBillingGroupMetadata
  -- * Lenses
  , bgmCreationDate
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Additional information about the billing group.
--
-- /See:/ 'mkBillingGroupMetadata' smart constructor.
newtype BillingGroupMetadata = BillingGroupMetadata'
  { creationDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the billing group was created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype Core.NFData

-- | Creates a 'BillingGroupMetadata' value with any optional fields omitted.
mkBillingGroupMetadata
    :: BillingGroupMetadata
mkBillingGroupMetadata
  = BillingGroupMetadata'{creationDate = Core.Nothing}

-- | The date the billing group was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgmCreationDate :: Lens.Lens' BillingGroupMetadata (Core.Maybe Core.NominalDiffTime)
bgmCreationDate = Lens.field @"creationDate"
{-# INLINEABLE bgmCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

instance Core.FromJSON BillingGroupMetadata where
        parseJSON
          = Core.withObject "BillingGroupMetadata" Core.$
              \ x -> BillingGroupMetadata' Core.<$> (x Core..:? "creationDate")
