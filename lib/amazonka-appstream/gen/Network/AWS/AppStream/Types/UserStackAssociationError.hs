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
    usaeUserStackAssociation,
    usaeErrorCode,
    usaeErrorMessage,
  )
where

import Network.AWS.AppStream.Types.UserStackAssociation
import Network.AWS.AppStream.Types.UserStackAssociationErrorCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the error that is returned when a user can’t be associated with or disassociated from a stack.
--
-- /See:/ 'mkUserStackAssociationError' smart constructor.
data UserStackAssociationError = UserStackAssociationError'
  { -- | Information about the user and associated stack.
    userStackAssociation :: Lude.Maybe UserStackAssociation,
    -- | The error code for the error that is returned when a user can’t be associated with or disassociated from a stack.
    errorCode :: Lude.Maybe UserStackAssociationErrorCode,
    -- | The error message for the error that is returned when a user can’t be associated with or disassociated from a stack.
    errorMessage :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UserStackAssociationError' with the minimum fields required to make a request.
--
-- * 'userStackAssociation' - Information about the user and associated stack.
-- * 'errorCode' - The error code for the error that is returned when a user can’t be associated with or disassociated from a stack.
-- * 'errorMessage' - The error message for the error that is returned when a user can’t be associated with or disassociated from a stack.
mkUserStackAssociationError ::
  UserStackAssociationError
mkUserStackAssociationError =
  UserStackAssociationError'
    { userStackAssociation = Lude.Nothing,
      errorCode = Lude.Nothing,
      errorMessage = Lude.Nothing
    }

-- | Information about the user and associated stack.
--
-- /Note:/ Consider using 'userStackAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaeUserStackAssociation :: Lens.Lens' UserStackAssociationError (Lude.Maybe UserStackAssociation)
usaeUserStackAssociation = Lens.lens (userStackAssociation :: UserStackAssociationError -> Lude.Maybe UserStackAssociation) (\s a -> s {userStackAssociation = a} :: UserStackAssociationError)
{-# DEPRECATED usaeUserStackAssociation "Use generic-lens or generic-optics with 'userStackAssociation' instead." #-}

-- | The error code for the error that is returned when a user can’t be associated with or disassociated from a stack.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaeErrorCode :: Lens.Lens' UserStackAssociationError (Lude.Maybe UserStackAssociationErrorCode)
usaeErrorCode = Lens.lens (errorCode :: UserStackAssociationError -> Lude.Maybe UserStackAssociationErrorCode) (\s a -> s {errorCode = a} :: UserStackAssociationError)
{-# DEPRECATED usaeErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The error message for the error that is returned when a user can’t be associated with or disassociated from a stack.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaeErrorMessage :: Lens.Lens' UserStackAssociationError (Lude.Maybe Lude.Text)
usaeErrorMessage = Lens.lens (errorMessage :: UserStackAssociationError -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: UserStackAssociationError)
{-# DEPRECATED usaeErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance Lude.FromJSON UserStackAssociationError where
  parseJSON =
    Lude.withObject
      "UserStackAssociationError"
      ( \x ->
          UserStackAssociationError'
            Lude.<$> (x Lude..:? "UserStackAssociation")
            Lude.<*> (x Lude..:? "ErrorCode")
            Lude.<*> (x Lude..:? "ErrorMessage")
      )
