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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.UserData where

import Amazonka.AlexaBusiness.Types.EnrollmentStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information related to a user.
--
-- /See:/ 'newUserData' smart constructor.
data UserData = UserData'
  { -- | The email of a user.
    email :: Prelude.Maybe Prelude.Text,
    -- | The enrollment ARN of a user.
    enrollmentId :: Prelude.Maybe Prelude.Text,
    -- | The enrollment status of a user.
    enrollmentStatus :: Prelude.Maybe EnrollmentStatus,
    -- | The first name of a user.
    firstName :: Prelude.Maybe Prelude.Text,
    -- | The last name of a user.
    lastName :: Prelude.Maybe Prelude.Text,
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
-- 'email', 'userData_email' - The email of a user.
--
-- 'enrollmentId', 'userData_enrollmentId' - The enrollment ARN of a user.
--
-- 'enrollmentStatus', 'userData_enrollmentStatus' - The enrollment status of a user.
--
-- 'firstName', 'userData_firstName' - The first name of a user.
--
-- 'lastName', 'userData_lastName' - The last name of a user.
--
-- 'userArn', 'userData_userArn' - The ARN of a user.
newUserData ::
  UserData
newUserData =
  UserData'
    { email = Prelude.Nothing,
      enrollmentId = Prelude.Nothing,
      enrollmentStatus = Prelude.Nothing,
      firstName = Prelude.Nothing,
      lastName = Prelude.Nothing,
      userArn = Prelude.Nothing
    }

-- | The email of a user.
userData_email :: Lens.Lens' UserData (Prelude.Maybe Prelude.Text)
userData_email = Lens.lens (\UserData' {email} -> email) (\s@UserData' {} a -> s {email = a} :: UserData)

-- | The enrollment ARN of a user.
userData_enrollmentId :: Lens.Lens' UserData (Prelude.Maybe Prelude.Text)
userData_enrollmentId = Lens.lens (\UserData' {enrollmentId} -> enrollmentId) (\s@UserData' {} a -> s {enrollmentId = a} :: UserData)

-- | The enrollment status of a user.
userData_enrollmentStatus :: Lens.Lens' UserData (Prelude.Maybe EnrollmentStatus)
userData_enrollmentStatus = Lens.lens (\UserData' {enrollmentStatus} -> enrollmentStatus) (\s@UserData' {} a -> s {enrollmentStatus = a} :: UserData)

-- | The first name of a user.
userData_firstName :: Lens.Lens' UserData (Prelude.Maybe Prelude.Text)
userData_firstName = Lens.lens (\UserData' {firstName} -> firstName) (\s@UserData' {} a -> s {firstName = a} :: UserData)

-- | The last name of a user.
userData_lastName :: Lens.Lens' UserData (Prelude.Maybe Prelude.Text)
userData_lastName = Lens.lens (\UserData' {lastName} -> lastName) (\s@UserData' {} a -> s {lastName = a} :: UserData)

-- | The ARN of a user.
userData_userArn :: Lens.Lens' UserData (Prelude.Maybe Prelude.Text)
userData_userArn = Lens.lens (\UserData' {userArn} -> userArn) (\s@UserData' {} a -> s {userArn = a} :: UserData)

instance Data.FromJSON UserData where
  parseJSON =
    Data.withObject
      "UserData"
      ( \x ->
          UserData'
            Prelude.<$> (x Data..:? "Email")
            Prelude.<*> (x Data..:? "EnrollmentId")
            Prelude.<*> (x Data..:? "EnrollmentStatus")
            Prelude.<*> (x Data..:? "FirstName")
            Prelude.<*> (x Data..:? "LastName")
            Prelude.<*> (x Data..:? "UserArn")
      )

instance Prelude.Hashable UserData where
  hashWithSalt _salt UserData' {..} =
    _salt
      `Prelude.hashWithSalt` email
      `Prelude.hashWithSalt` enrollmentId
      `Prelude.hashWithSalt` enrollmentStatus
      `Prelude.hashWithSalt` firstName
      `Prelude.hashWithSalt` lastName
      `Prelude.hashWithSalt` userArn

instance Prelude.NFData UserData where
  rnf UserData' {..} =
    Prelude.rnf email `Prelude.seq`
      Prelude.rnf enrollmentId `Prelude.seq`
        Prelude.rnf enrollmentStatus `Prelude.seq`
          Prelude.rnf firstName `Prelude.seq`
            Prelude.rnf lastName `Prelude.seq`
              Prelude.rnf userArn
