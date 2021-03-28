{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SuccessfulInstanceCreditSpecificationItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.SuccessfulInstanceCreditSpecificationItem
  ( SuccessfulInstanceCreditSpecificationItem (..)
  -- * Smart constructor
  , mkSuccessfulInstanceCreditSpecificationItem
  -- * Lenses
  , sicsiInstanceId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the burstable performance instance whose credit option for CPU usage was successfully modified.
--
-- /See:/ 'mkSuccessfulInstanceCreditSpecificationItem' smart constructor.
newtype SuccessfulInstanceCreditSpecificationItem = SuccessfulInstanceCreditSpecificationItem'
  { instanceId :: Core.Maybe Core.Text
    -- ^ The ID of the instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SuccessfulInstanceCreditSpecificationItem' value with any optional fields omitted.
mkSuccessfulInstanceCreditSpecificationItem
    :: SuccessfulInstanceCreditSpecificationItem
mkSuccessfulInstanceCreditSpecificationItem
  = SuccessfulInstanceCreditSpecificationItem'{instanceId =
                                                 Core.Nothing}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sicsiInstanceId :: Lens.Lens' SuccessfulInstanceCreditSpecificationItem (Core.Maybe Core.Text)
sicsiInstanceId = Lens.field @"instanceId"
{-# INLINEABLE sicsiInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

instance Core.FromXML SuccessfulInstanceCreditSpecificationItem
         where
        parseXML x
          = SuccessfulInstanceCreditSpecificationItem' Core.<$>
              (x Core..@? "instanceId")
