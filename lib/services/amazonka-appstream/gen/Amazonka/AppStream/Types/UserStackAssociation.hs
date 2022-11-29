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
-- Module      : Amazonka.AppStream.Types.UserStackAssociation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.UserStackAssociation where

import Amazonka.AppStream.Types.AuthenticationType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a user in the user pool and the associated stack.
--
-- /See:/ 'newUserStackAssociation' smart constructor.
data UserStackAssociation = UserStackAssociation'
  { -- | Specifies whether a welcome email is sent to a user after the user is
    -- created in the user pool.
    sendEmailNotification :: Prelude.Maybe Prelude.Bool,
    -- | The name of the stack that is associated with the user.
    stackName :: Prelude.Text,
    -- | The email address of the user who is associated with the stack.
    --
    -- Users\' email addresses are case-sensitive.
    userName :: Core.Sensitive Prelude.Text,
    -- | The authentication type for the user.
    authenticationType :: AuthenticationType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserStackAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sendEmailNotification', 'userStackAssociation_sendEmailNotification' - Specifies whether a welcome email is sent to a user after the user is
-- created in the user pool.
--
-- 'stackName', 'userStackAssociation_stackName' - The name of the stack that is associated with the user.
--
-- 'userName', 'userStackAssociation_userName' - The email address of the user who is associated with the stack.
--
-- Users\' email addresses are case-sensitive.
--
-- 'authenticationType', 'userStackAssociation_authenticationType' - The authentication type for the user.
newUserStackAssociation ::
  -- | 'stackName'
  Prelude.Text ->
  -- | 'userName'
  Prelude.Text ->
  -- | 'authenticationType'
  AuthenticationType ->
  UserStackAssociation
newUserStackAssociation
  pStackName_
  pUserName_
  pAuthenticationType_ =
    UserStackAssociation'
      { sendEmailNotification =
          Prelude.Nothing,
        stackName = pStackName_,
        userName = Core._Sensitive Lens.# pUserName_,
        authenticationType = pAuthenticationType_
      }

-- | Specifies whether a welcome email is sent to a user after the user is
-- created in the user pool.
userStackAssociation_sendEmailNotification :: Lens.Lens' UserStackAssociation (Prelude.Maybe Prelude.Bool)
userStackAssociation_sendEmailNotification = Lens.lens (\UserStackAssociation' {sendEmailNotification} -> sendEmailNotification) (\s@UserStackAssociation' {} a -> s {sendEmailNotification = a} :: UserStackAssociation)

-- | The name of the stack that is associated with the user.
userStackAssociation_stackName :: Lens.Lens' UserStackAssociation Prelude.Text
userStackAssociation_stackName = Lens.lens (\UserStackAssociation' {stackName} -> stackName) (\s@UserStackAssociation' {} a -> s {stackName = a} :: UserStackAssociation)

-- | The email address of the user who is associated with the stack.
--
-- Users\' email addresses are case-sensitive.
userStackAssociation_userName :: Lens.Lens' UserStackAssociation Prelude.Text
userStackAssociation_userName = Lens.lens (\UserStackAssociation' {userName} -> userName) (\s@UserStackAssociation' {} a -> s {userName = a} :: UserStackAssociation) Prelude.. Core._Sensitive

-- | The authentication type for the user.
userStackAssociation_authenticationType :: Lens.Lens' UserStackAssociation AuthenticationType
userStackAssociation_authenticationType = Lens.lens (\UserStackAssociation' {authenticationType} -> authenticationType) (\s@UserStackAssociation' {} a -> s {authenticationType = a} :: UserStackAssociation)

instance Core.FromJSON UserStackAssociation where
  parseJSON =
    Core.withObject
      "UserStackAssociation"
      ( \x ->
          UserStackAssociation'
            Prelude.<$> (x Core..:? "SendEmailNotification")
            Prelude.<*> (x Core..: "StackName")
            Prelude.<*> (x Core..: "UserName")
            Prelude.<*> (x Core..: "AuthenticationType")
      )

instance Prelude.Hashable UserStackAssociation where
  hashWithSalt _salt UserStackAssociation' {..} =
    _salt `Prelude.hashWithSalt` sendEmailNotification
      `Prelude.hashWithSalt` stackName
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` authenticationType

instance Prelude.NFData UserStackAssociation where
  rnf UserStackAssociation' {..} =
    Prelude.rnf sendEmailNotification
      `Prelude.seq` Prelude.rnf stackName
      `Prelude.seq` Prelude.rnf userName
      `Prelude.seq` Prelude.rnf authenticationType

instance Core.ToJSON UserStackAssociation where
  toJSON UserStackAssociation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SendEmailNotification" Core..=)
              Prelude.<$> sendEmailNotification,
            Prelude.Just ("StackName" Core..= stackName),
            Prelude.Just ("UserName" Core..= userName),
            Prelude.Just
              ("AuthenticationType" Core..= authenticationType)
          ]
      )
