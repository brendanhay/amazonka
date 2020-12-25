{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.UnsuccessfulInstanceCreditSpecificationItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.UnsuccessfulInstanceCreditSpecificationItem
  ( UnsuccessfulInstanceCreditSpecificationItem (..),

    -- * Smart constructor
    mkUnsuccessfulInstanceCreditSpecificationItem,

    -- * Lenses
    uicsiError,
    uicsiInstanceId,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.UnsuccessfulInstanceCreditSpecificationItemError as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the burstable performance instance whose credit option for CPU usage was not modified.
--
-- /See:/ 'mkUnsuccessfulInstanceCreditSpecificationItem' smart constructor.
data UnsuccessfulInstanceCreditSpecificationItem = UnsuccessfulInstanceCreditSpecificationItem'
  { -- | The applicable error for the burstable performance instance whose credit option for CPU usage was not modified.
    error :: Core.Maybe Types.UnsuccessfulInstanceCreditSpecificationItemError,
    -- | The ID of the instance.
    instanceId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UnsuccessfulInstanceCreditSpecificationItem' value with any optional fields omitted.
mkUnsuccessfulInstanceCreditSpecificationItem ::
  UnsuccessfulInstanceCreditSpecificationItem
mkUnsuccessfulInstanceCreditSpecificationItem =
  UnsuccessfulInstanceCreditSpecificationItem'
    { error =
        Core.Nothing,
      instanceId = Core.Nothing
    }

-- | The applicable error for the burstable performance instance whose credit option for CPU usage was not modified.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uicsiError :: Lens.Lens' UnsuccessfulInstanceCreditSpecificationItem (Core.Maybe Types.UnsuccessfulInstanceCreditSpecificationItemError)
uicsiError = Lens.field @"error"
{-# DEPRECATED uicsiError "Use generic-lens or generic-optics with 'error' instead." #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uicsiInstanceId :: Lens.Lens' UnsuccessfulInstanceCreditSpecificationItem (Core.Maybe Types.String)
uicsiInstanceId = Lens.field @"instanceId"
{-# DEPRECATED uicsiInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Core.FromXML UnsuccessfulInstanceCreditSpecificationItem where
  parseXML x =
    UnsuccessfulInstanceCreditSpecificationItem'
      Core.<$> (x Core..@? "error") Core.<*> (x Core..@? "instanceId")
