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
-- Module      : Network.AWS.AppStream.Types.UserStackAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.UserStackAssociation where

import Network.AWS.AppStream.Types.AuthenticationType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    userName :: Prelude.Sensitive Prelude.Text,
    -- | The authentication type for the user.
    authenticationType :: AuthenticationType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        userName = Prelude._Sensitive Lens.# pUserName_,
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
userStackAssociation_userName = Lens.lens (\UserStackAssociation' {userName} -> userName) (\s@UserStackAssociation' {} a -> s {userName = a} :: UserStackAssociation) Prelude.. Prelude._Sensitive

-- | The authentication type for the user.
userStackAssociation_authenticationType :: Lens.Lens' UserStackAssociation AuthenticationType
userStackAssociation_authenticationType = Lens.lens (\UserStackAssociation' {authenticationType} -> authenticationType) (\s@UserStackAssociation' {} a -> s {authenticationType = a} :: UserStackAssociation)

instance Prelude.FromJSON UserStackAssociation where
  parseJSON =
    Prelude.withObject
      "UserStackAssociation"
      ( \x ->
          UserStackAssociation'
            Prelude.<$> (x Prelude..:? "SendEmailNotification")
            Prelude.<*> (x Prelude..: "StackName")
            Prelude.<*> (x Prelude..: "UserName")
            Prelude.<*> (x Prelude..: "AuthenticationType")
      )

instance Prelude.Hashable UserStackAssociation

instance Prelude.NFData UserStackAssociation

instance Prelude.ToJSON UserStackAssociation where
  toJSON UserStackAssociation' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SendEmailNotification" Prelude..=)
              Prelude.<$> sendEmailNotification,
            Prelude.Just ("StackName" Prelude..= stackName),
            Prelude.Just ("UserName" Prelude..= userName),
            Prelude.Just
              ( "AuthenticationType"
                  Prelude..= authenticationType
              )
          ]
      )
