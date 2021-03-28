{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AssociationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.AssociationStatus
  ( AssociationStatus (..)
  -- * Smart constructor
  , mkAssociationStatus
  -- * Lenses
  , asCode
  , asMessage
  ) where

import qualified Network.AWS.EC2.Types.AssociationStatusCode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the state of a target network association.
--
-- /See:/ 'mkAssociationStatus' smart constructor.
data AssociationStatus = AssociationStatus'
  { code :: Core.Maybe Types.AssociationStatusCode
    -- ^ The state of the target network association.
  , message :: Core.Maybe Core.Text
    -- ^ A message about the status of the target network association, if applicable.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociationStatus' value with any optional fields omitted.
mkAssociationStatus
    :: AssociationStatus
mkAssociationStatus
  = AssociationStatus'{code = Core.Nothing, message = Core.Nothing}

-- | The state of the target network association.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asCode :: Lens.Lens' AssociationStatus (Core.Maybe Types.AssociationStatusCode)
asCode = Lens.field @"code"
{-# INLINEABLE asCode #-}
{-# DEPRECATED code "Use generic-lens or generic-optics with 'code' instead"  #-}

-- | A message about the status of the target network association, if applicable.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asMessage :: Lens.Lens' AssociationStatus (Core.Maybe Core.Text)
asMessage = Lens.field @"message"
{-# INLINEABLE asMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

instance Core.FromXML AssociationStatus where
        parseXML x
          = AssociationStatus' Core.<$>
              (x Core..@? "code") Core.<*> x Core..@? "message"
