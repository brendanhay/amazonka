{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.UserStackAssociationError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.UserStackAssociationError where

import Network.AWS.AppStream.Types.UserStackAssociation
import Network.AWS.AppStream.Types.UserStackAssociationErrorCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the error that is returned when a user can’t be associated
-- with or disassociated from a stack.
--
-- /See:/ 'newUserStackAssociationError' smart constructor.
data UserStackAssociationError = UserStackAssociationError'
  { -- | Information about the user and associated stack.
    userStackAssociation :: Prelude.Maybe UserStackAssociation,
    -- | The error message for the error that is returned when a user can’t be
    -- associated with or disassociated from a stack.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The error code for the error that is returned when a user can’t be
    -- associated with or disassociated from a stack.
    errorCode :: Prelude.Maybe UserStackAssociationErrorCode
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UserStackAssociationError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userStackAssociation', 'userStackAssociationError_userStackAssociation' - Information about the user and associated stack.
--
-- 'errorMessage', 'userStackAssociationError_errorMessage' - The error message for the error that is returned when a user can’t be
-- associated with or disassociated from a stack.
--
-- 'errorCode', 'userStackAssociationError_errorCode' - The error code for the error that is returned when a user can’t be
-- associated with or disassociated from a stack.
newUserStackAssociationError ::
  UserStackAssociationError
newUserStackAssociationError =
  UserStackAssociationError'
    { userStackAssociation =
        Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | Information about the user and associated stack.
userStackAssociationError_userStackAssociation :: Lens.Lens' UserStackAssociationError (Prelude.Maybe UserStackAssociation)
userStackAssociationError_userStackAssociation = Lens.lens (\UserStackAssociationError' {userStackAssociation} -> userStackAssociation) (\s@UserStackAssociationError' {} a -> s {userStackAssociation = a} :: UserStackAssociationError)

-- | The error message for the error that is returned when a user can’t be
-- associated with or disassociated from a stack.
userStackAssociationError_errorMessage :: Lens.Lens' UserStackAssociationError (Prelude.Maybe Prelude.Text)
userStackAssociationError_errorMessage = Lens.lens (\UserStackAssociationError' {errorMessage} -> errorMessage) (\s@UserStackAssociationError' {} a -> s {errorMessage = a} :: UserStackAssociationError)

-- | The error code for the error that is returned when a user can’t be
-- associated with or disassociated from a stack.
userStackAssociationError_errorCode :: Lens.Lens' UserStackAssociationError (Prelude.Maybe UserStackAssociationErrorCode)
userStackAssociationError_errorCode = Lens.lens (\UserStackAssociationError' {errorCode} -> errorCode) (\s@UserStackAssociationError' {} a -> s {errorCode = a} :: UserStackAssociationError)

instance Prelude.FromJSON UserStackAssociationError where
  parseJSON =
    Prelude.withObject
      "UserStackAssociationError"
      ( \x ->
          UserStackAssociationError'
            Prelude.<$> (x Prelude..:? "UserStackAssociation")
            Prelude.<*> (x Prelude..:? "ErrorMessage")
            Prelude.<*> (x Prelude..:? "ErrorCode")
      )

instance Prelude.Hashable UserStackAssociationError

instance Prelude.NFData UserStackAssociationError
