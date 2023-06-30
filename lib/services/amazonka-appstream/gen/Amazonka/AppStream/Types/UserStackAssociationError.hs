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
-- Module      : Amazonka.AppStream.Types.UserStackAssociationError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.UserStackAssociationError where

import Amazonka.AppStream.Types.UserStackAssociation
import Amazonka.AppStream.Types.UserStackAssociationErrorCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the error that is returned when a user can’t be associated
-- with or disassociated from a stack.
--
-- /See:/ 'newUserStackAssociationError' smart constructor.
data UserStackAssociationError = UserStackAssociationError'
  { -- | The error code for the error that is returned when a user can’t be
    -- associated with or disassociated from a stack.
    errorCode :: Prelude.Maybe UserStackAssociationErrorCode,
    -- | The error message for the error that is returned when a user can’t be
    -- associated with or disassociated from a stack.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | Information about the user and associated stack.
    userStackAssociation :: Prelude.Maybe UserStackAssociation
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserStackAssociationError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'userStackAssociationError_errorCode' - The error code for the error that is returned when a user can’t be
-- associated with or disassociated from a stack.
--
-- 'errorMessage', 'userStackAssociationError_errorMessage' - The error message for the error that is returned when a user can’t be
-- associated with or disassociated from a stack.
--
-- 'userStackAssociation', 'userStackAssociationError_userStackAssociation' - Information about the user and associated stack.
newUserStackAssociationError ::
  UserStackAssociationError
newUserStackAssociationError =
  UserStackAssociationError'
    { errorCode =
        Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      userStackAssociation = Prelude.Nothing
    }

-- | The error code for the error that is returned when a user can’t be
-- associated with or disassociated from a stack.
userStackAssociationError_errorCode :: Lens.Lens' UserStackAssociationError (Prelude.Maybe UserStackAssociationErrorCode)
userStackAssociationError_errorCode = Lens.lens (\UserStackAssociationError' {errorCode} -> errorCode) (\s@UserStackAssociationError' {} a -> s {errorCode = a} :: UserStackAssociationError)

-- | The error message for the error that is returned when a user can’t be
-- associated with or disassociated from a stack.
userStackAssociationError_errorMessage :: Lens.Lens' UserStackAssociationError (Prelude.Maybe Prelude.Text)
userStackAssociationError_errorMessage = Lens.lens (\UserStackAssociationError' {errorMessage} -> errorMessage) (\s@UserStackAssociationError' {} a -> s {errorMessage = a} :: UserStackAssociationError)

-- | Information about the user and associated stack.
userStackAssociationError_userStackAssociation :: Lens.Lens' UserStackAssociationError (Prelude.Maybe UserStackAssociation)
userStackAssociationError_userStackAssociation = Lens.lens (\UserStackAssociationError' {userStackAssociation} -> userStackAssociation) (\s@UserStackAssociationError' {} a -> s {userStackAssociation = a} :: UserStackAssociationError)

instance Data.FromJSON UserStackAssociationError where
  parseJSON =
    Data.withObject
      "UserStackAssociationError"
      ( \x ->
          UserStackAssociationError'
            Prelude.<$> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "UserStackAssociation")
      )

instance Prelude.Hashable UserStackAssociationError where
  hashWithSalt _salt UserStackAssociationError' {..} =
    _salt
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` userStackAssociation

instance Prelude.NFData UserStackAssociationError where
  rnf UserStackAssociationError' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf userStackAssociation
