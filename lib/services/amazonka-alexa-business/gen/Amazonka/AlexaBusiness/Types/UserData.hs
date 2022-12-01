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
-- Module      : Amazonka.AlexaBusiness.Types.UserData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.UserData where

import Amazonka.AlexaBusiness.Types.EnrollmentStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information related to a user.
--
-- /See:/ 'newUserData' smart constructor.
data UserData = UserData'
  { -- | The first name of a user.
    firstName :: Prelude.Maybe Prelude.Text,
    -- | The email of a user.
    email :: Prelude.Maybe Prelude.Text,
    -- | The enrollment status of a user.
    enrollmentStatus :: Prelude.Maybe EnrollmentStatus,
    -- | The last name of a user.
    lastName :: Prelude.Maybe Prelude.Text,
    -- | The enrollment ARN of a user.
    enrollmentId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of a user.
    userArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firstName', 'userData_firstName' - The first name of a user.
--
-- 'email', 'userData_email' - The email of a user.
--
-- 'enrollmentStatus', 'userData_enrollmentStatus' - The enrollment status of a user.
--
-- 'lastName', 'userData_lastName' - The last name of a user.
--
-- 'enrollmentId', 'userData_enrollmentId' - The enrollment ARN of a user.
--
-- 'userArn', 'userData_userArn' - The ARN of a user.
newUserData ::
  UserData
newUserData =
  UserData'
    { firstName = Prelude.Nothing,
      email = Prelude.Nothing,
      enrollmentStatus = Prelude.Nothing,
      lastName = Prelude.Nothing,
      enrollmentId = Prelude.Nothing,
      userArn = Prelude.Nothing
    }

-- | The first name of a user.
userData_firstName :: Lens.Lens' UserData (Prelude.Maybe Prelude.Text)
userData_firstName = Lens.lens (\UserData' {firstName} -> firstName) (\s@UserData' {} a -> s {firstName = a} :: UserData)

-- | The email of a user.
userData_email :: Lens.Lens' UserData (Prelude.Maybe Prelude.Text)
userData_email = Lens.lens (\UserData' {email} -> email) (\s@UserData' {} a -> s {email = a} :: UserData)

-- | The enrollment status of a user.
userData_enrollmentStatus :: Lens.Lens' UserData (Prelude.Maybe EnrollmentStatus)
userData_enrollmentStatus = Lens.lens (\UserData' {enrollmentStatus} -> enrollmentStatus) (\s@UserData' {} a -> s {enrollmentStatus = a} :: UserData)

-- | The last name of a user.
userData_lastName :: Lens.Lens' UserData (Prelude.Maybe Prelude.Text)
userData_lastName = Lens.lens (\UserData' {lastName} -> lastName) (\s@UserData' {} a -> s {lastName = a} :: UserData)

-- | The enrollment ARN of a user.
userData_enrollmentId :: Lens.Lens' UserData (Prelude.Maybe Prelude.Text)
userData_enrollmentId = Lens.lens (\UserData' {enrollmentId} -> enrollmentId) (\s@UserData' {} a -> s {enrollmentId = a} :: UserData)

-- | The ARN of a user.
userData_userArn :: Lens.Lens' UserData (Prelude.Maybe Prelude.Text)
userData_userArn = Lens.lens (\UserData' {userArn} -> userArn) (\s@UserData' {} a -> s {userArn = a} :: UserData)

instance Core.FromJSON UserData where
  parseJSON =
    Core.withObject
      "UserData"
      ( \x ->
          UserData'
            Prelude.<$> (x Core..:? "FirstName")
            Prelude.<*> (x Core..:? "Email")
            Prelude.<*> (x Core..:? "EnrollmentStatus")
            Prelude.<*> (x Core..:? "LastName")
            Prelude.<*> (x Core..:? "EnrollmentId")
            Prelude.<*> (x Core..:? "UserArn")
      )

instance Prelude.Hashable UserData where
  hashWithSalt _salt UserData' {..} =
    _salt `Prelude.hashWithSalt` firstName
      `Prelude.hashWithSalt` email
      `Prelude.hashWithSalt` enrollmentStatus
      `Prelude.hashWithSalt` lastName
      `Prelude.hashWithSalt` enrollmentId
      `Prelude.hashWithSalt` userArn

instance Prelude.NFData UserData where
  rnf UserData' {..} =
    Prelude.rnf firstName
      `Prelude.seq` Prelude.rnf email
      `Prelude.seq` Prelude.rnf enrollmentStatus
      `Prelude.seq` Prelude.rnf lastName
      `Prelude.seq` Prelude.rnf enrollmentId
      `Prelude.seq` Prelude.rnf userArn
