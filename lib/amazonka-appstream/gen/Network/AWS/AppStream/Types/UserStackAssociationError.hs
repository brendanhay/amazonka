{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.UserStackAssociationError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.UserStackAssociationError
  ( UserStackAssociationError (..),

    -- * Smart constructor
    mkUserStackAssociationError,

    -- * Lenses
    usaeErrorCode,
    usaeErrorMessage,
    usaeUserStackAssociation,
  )
where

import qualified Network.AWS.AppStream.Types.String as Types
import qualified Network.AWS.AppStream.Types.UserStackAssociation as Types
import qualified Network.AWS.AppStream.Types.UserStackAssociationErrorCode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the error that is returned when a user can’t be associated with or disassociated from a stack.
--
-- /See:/ 'mkUserStackAssociationError' smart constructor.
data UserStackAssociationError = UserStackAssociationError'
  { -- | The error code for the error that is returned when a user can’t be associated with or disassociated from a stack.
    errorCode :: Core.Maybe Types.UserStackAssociationErrorCode,
    -- | The error message for the error that is returned when a user can’t be associated with or disassociated from a stack.
    errorMessage :: Core.Maybe Types.String,
    -- | Information about the user and associated stack.
    userStackAssociation :: Core.Maybe Types.UserStackAssociation
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UserStackAssociationError' value with any optional fields omitted.
mkUserStackAssociationError ::
  UserStackAssociationError
mkUserStackAssociationError =
  UserStackAssociationError'
    { errorCode = Core.Nothing,
      errorMessage = Core.Nothing,
      userStackAssociation = Core.Nothing
    }

-- | The error code for the error that is returned when a user can’t be associated with or disassociated from a stack.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaeErrorCode :: Lens.Lens' UserStackAssociationError (Core.Maybe Types.UserStackAssociationErrorCode)
usaeErrorCode = Lens.field @"errorCode"
{-# DEPRECATED usaeErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The error message for the error that is returned when a user can’t be associated with or disassociated from a stack.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaeErrorMessage :: Lens.Lens' UserStackAssociationError (Core.Maybe Types.String)
usaeErrorMessage = Lens.field @"errorMessage"
{-# DEPRECATED usaeErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | Information about the user and associated stack.
--
-- /Note:/ Consider using 'userStackAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaeUserStackAssociation :: Lens.Lens' UserStackAssociationError (Core.Maybe Types.UserStackAssociation)
usaeUserStackAssociation = Lens.field @"userStackAssociation"
{-# DEPRECATED usaeUserStackAssociation "Use generic-lens or generic-optics with 'userStackAssociation' instead." #-}

instance Core.FromJSON UserStackAssociationError where
  parseJSON =
    Core.withObject "UserStackAssociationError" Core.$
      \x ->
        UserStackAssociationError'
          Core.<$> (x Core..:? "ErrorCode")
          Core.<*> (x Core..:? "ErrorMessage")
          Core.<*> (x Core..:? "UserStackAssociation")
