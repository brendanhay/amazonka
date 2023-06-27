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
-- Module      : Amazonka.GuardDuty.Types.LoginAttribute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.LoginAttribute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the login attempts.
--
-- /See:/ 'newLoginAttribute' smart constructor.
data LoginAttribute = LoginAttribute'
  { -- | Indicates the application name used to attempt log in.
    application :: Prelude.Maybe Prelude.Text,
    -- | Represents the sum of failed (unsuccessful) login attempts made to
    -- establish a connection to the database instance.
    failedLoginAttempts :: Prelude.Maybe Prelude.Int,
    -- | Represents the sum of successful connections (a correct combination of
    -- login attributes) made to the database instance by the actor.
    successfulLoginAttempts :: Prelude.Maybe Prelude.Int,
    -- | Indicates the user name which attempted to log in.
    user :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoginAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'application', 'loginAttribute_application' - Indicates the application name used to attempt log in.
--
-- 'failedLoginAttempts', 'loginAttribute_failedLoginAttempts' - Represents the sum of failed (unsuccessful) login attempts made to
-- establish a connection to the database instance.
--
-- 'successfulLoginAttempts', 'loginAttribute_successfulLoginAttempts' - Represents the sum of successful connections (a correct combination of
-- login attributes) made to the database instance by the actor.
--
-- 'user', 'loginAttribute_user' - Indicates the user name which attempted to log in.
newLoginAttribute ::
  LoginAttribute
newLoginAttribute =
  LoginAttribute'
    { application = Prelude.Nothing,
      failedLoginAttempts = Prelude.Nothing,
      successfulLoginAttempts = Prelude.Nothing,
      user = Prelude.Nothing
    }

-- | Indicates the application name used to attempt log in.
loginAttribute_application :: Lens.Lens' LoginAttribute (Prelude.Maybe Prelude.Text)
loginAttribute_application = Lens.lens (\LoginAttribute' {application} -> application) (\s@LoginAttribute' {} a -> s {application = a} :: LoginAttribute)

-- | Represents the sum of failed (unsuccessful) login attempts made to
-- establish a connection to the database instance.
loginAttribute_failedLoginAttempts :: Lens.Lens' LoginAttribute (Prelude.Maybe Prelude.Int)
loginAttribute_failedLoginAttempts = Lens.lens (\LoginAttribute' {failedLoginAttempts} -> failedLoginAttempts) (\s@LoginAttribute' {} a -> s {failedLoginAttempts = a} :: LoginAttribute)

-- | Represents the sum of successful connections (a correct combination of
-- login attributes) made to the database instance by the actor.
loginAttribute_successfulLoginAttempts :: Lens.Lens' LoginAttribute (Prelude.Maybe Prelude.Int)
loginAttribute_successfulLoginAttempts = Lens.lens (\LoginAttribute' {successfulLoginAttempts} -> successfulLoginAttempts) (\s@LoginAttribute' {} a -> s {successfulLoginAttempts = a} :: LoginAttribute)

-- | Indicates the user name which attempted to log in.
loginAttribute_user :: Lens.Lens' LoginAttribute (Prelude.Maybe Prelude.Text)
loginAttribute_user = Lens.lens (\LoginAttribute' {user} -> user) (\s@LoginAttribute' {} a -> s {user = a} :: LoginAttribute)

instance Data.FromJSON LoginAttribute where
  parseJSON =
    Data.withObject
      "LoginAttribute"
      ( \x ->
          LoginAttribute'
            Prelude.<$> (x Data..:? "application")
            Prelude.<*> (x Data..:? "failedLoginAttempts")
            Prelude.<*> (x Data..:? "successfulLoginAttempts")
            Prelude.<*> (x Data..:? "user")
      )

instance Prelude.Hashable LoginAttribute where
  hashWithSalt _salt LoginAttribute' {..} =
    _salt
      `Prelude.hashWithSalt` application
      `Prelude.hashWithSalt` failedLoginAttempts
      `Prelude.hashWithSalt` successfulLoginAttempts
      `Prelude.hashWithSalt` user

instance Prelude.NFData LoginAttribute where
  rnf LoginAttribute' {..} =
    Prelude.rnf application
      `Prelude.seq` Prelude.rnf failedLoginAttempts
      `Prelude.seq` Prelude.rnf successfulLoginAttempts
      `Prelude.seq` Prelude.rnf user
