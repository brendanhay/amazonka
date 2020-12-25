{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DeleteFleetError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DeleteFleetError
  ( DeleteFleetError (..),

    -- * Smart constructor
    mkDeleteFleetError,

    -- * Lenses
    dfeCode,
    dfeMessage,
  )
where

import qualified Network.AWS.EC2.Types.DeleteFleetErrorCode as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an EC2 Fleet error.
--
-- /See:/ 'mkDeleteFleetError' smart constructor.
data DeleteFleetError = DeleteFleetError'
  { -- | The error code.
    code :: Core.Maybe Types.DeleteFleetErrorCode,
    -- | The description for the error code.
    message :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFleetError' value with any optional fields omitted.
mkDeleteFleetError ::
  DeleteFleetError
mkDeleteFleetError =
  DeleteFleetError' {code = Core.Nothing, message = Core.Nothing}

-- | The error code.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfeCode :: Lens.Lens' DeleteFleetError (Core.Maybe Types.DeleteFleetErrorCode)
dfeCode = Lens.field @"code"
{-# DEPRECATED dfeCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The description for the error code.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfeMessage :: Lens.Lens' DeleteFleetError (Core.Maybe Types.String)
dfeMessage = Lens.field @"message"
{-# DEPRECATED dfeMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Core.FromXML DeleteFleetError where
  parseXML x =
    DeleteFleetError'
      Core.<$> (x Core..@? "code") Core.<*> (x Core..@? "message")
