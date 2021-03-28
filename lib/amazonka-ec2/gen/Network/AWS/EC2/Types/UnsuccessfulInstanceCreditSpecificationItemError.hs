{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.UnsuccessfulInstanceCreditSpecificationItemError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.UnsuccessfulInstanceCreditSpecificationItemError
  ( UnsuccessfulInstanceCreditSpecificationItemError (..)
  -- * Smart constructor
  , mkUnsuccessfulInstanceCreditSpecificationItemError
  -- * Lenses
  , uicsieCode
  , uicsieMessage
  ) where

import qualified Network.AWS.EC2.Types.UnsuccessfulInstanceCreditSpecificationErrorCode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the error for the burstable performance instance whose credit option for CPU usage was not modified.
--
-- /See:/ 'mkUnsuccessfulInstanceCreditSpecificationItemError' smart constructor.
data UnsuccessfulInstanceCreditSpecificationItemError = UnsuccessfulInstanceCreditSpecificationItemError'
  { code :: Core.Maybe Types.UnsuccessfulInstanceCreditSpecificationErrorCode
    -- ^ The error code.
  , message :: Core.Maybe Core.Text
    -- ^ The applicable error message.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UnsuccessfulInstanceCreditSpecificationItemError' value with any optional fields omitted.
mkUnsuccessfulInstanceCreditSpecificationItemError
    :: UnsuccessfulInstanceCreditSpecificationItemError
mkUnsuccessfulInstanceCreditSpecificationItemError
  = UnsuccessfulInstanceCreditSpecificationItemError'{code =
                                                        Core.Nothing,
                                                      message = Core.Nothing}

-- | The error code.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uicsieCode :: Lens.Lens' UnsuccessfulInstanceCreditSpecificationItemError (Core.Maybe Types.UnsuccessfulInstanceCreditSpecificationErrorCode)
uicsieCode = Lens.field @"code"
{-# INLINEABLE uicsieCode #-}
{-# DEPRECATED code "Use generic-lens or generic-optics with 'code' instead"  #-}

-- | The applicable error message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uicsieMessage :: Lens.Lens' UnsuccessfulInstanceCreditSpecificationItemError (Core.Maybe Core.Text)
uicsieMessage = Lens.field @"message"
{-# INLINEABLE uicsieMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

instance Core.FromXML
           UnsuccessfulInstanceCreditSpecificationItemError
         where
        parseXML x
          = UnsuccessfulInstanceCreditSpecificationItemError' Core.<$>
              (x Core..@? "code") Core.<*> x Core..@? "message"
