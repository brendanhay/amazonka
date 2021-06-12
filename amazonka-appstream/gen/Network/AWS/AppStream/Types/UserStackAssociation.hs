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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes a user in the user pool and the associated stack.
--
-- /See:/ 'newUserStackAssociation' smart constructor.
data UserStackAssociation = UserStackAssociation'
  { -- | Specifies whether a welcome email is sent to a user after the user is
    -- created in the user pool.
    sendEmailNotification :: Core.Maybe Core.Bool,
    -- | The name of the stack that is associated with the user.
    stackName :: Core.Text,
    -- | The email address of the user who is associated with the stack.
    --
    -- Users\' email addresses are case-sensitive.
    userName :: Core.Sensitive Core.Text,
    -- | The authentication type for the user.
    authenticationType :: AuthenticationType
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'userName'
  Core.Text ->
  -- | 'authenticationType'
  AuthenticationType ->
  UserStackAssociation
newUserStackAssociation
  pStackName_
  pUserName_
  pAuthenticationType_ =
    UserStackAssociation'
      { sendEmailNotification =
          Core.Nothing,
        stackName = pStackName_,
        userName = Core._Sensitive Lens.# pUserName_,
        authenticationType = pAuthenticationType_
      }

-- | Specifies whether a welcome email is sent to a user after the user is
-- created in the user pool.
userStackAssociation_sendEmailNotification :: Lens.Lens' UserStackAssociation (Core.Maybe Core.Bool)
userStackAssociation_sendEmailNotification = Lens.lens (\UserStackAssociation' {sendEmailNotification} -> sendEmailNotification) (\s@UserStackAssociation' {} a -> s {sendEmailNotification = a} :: UserStackAssociation)

-- | The name of the stack that is associated with the user.
userStackAssociation_stackName :: Lens.Lens' UserStackAssociation Core.Text
userStackAssociation_stackName = Lens.lens (\UserStackAssociation' {stackName} -> stackName) (\s@UserStackAssociation' {} a -> s {stackName = a} :: UserStackAssociation)

-- | The email address of the user who is associated with the stack.
--
-- Users\' email addresses are case-sensitive.
userStackAssociation_userName :: Lens.Lens' UserStackAssociation Core.Text
userStackAssociation_userName = Lens.lens (\UserStackAssociation' {userName} -> userName) (\s@UserStackAssociation' {} a -> s {userName = a} :: UserStackAssociation) Core.. Core._Sensitive

-- | The authentication type for the user.
userStackAssociation_authenticationType :: Lens.Lens' UserStackAssociation AuthenticationType
userStackAssociation_authenticationType = Lens.lens (\UserStackAssociation' {authenticationType} -> authenticationType) (\s@UserStackAssociation' {} a -> s {authenticationType = a} :: UserStackAssociation)

instance Core.FromJSON UserStackAssociation where
  parseJSON =
    Core.withObject
      "UserStackAssociation"
      ( \x ->
          UserStackAssociation'
            Core.<$> (x Core..:? "SendEmailNotification")
            Core.<*> (x Core..: "StackName")
            Core.<*> (x Core..: "UserName")
            Core.<*> (x Core..: "AuthenticationType")
      )

instance Core.Hashable UserStackAssociation

instance Core.NFData UserStackAssociation

instance Core.ToJSON UserStackAssociation where
  toJSON UserStackAssociation' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SendEmailNotification" Core..=)
              Core.<$> sendEmailNotification,
            Core.Just ("StackName" Core..= stackName),
            Core.Just ("UserName" Core..= userName),
            Core.Just
              ("AuthenticationType" Core..= authenticationType)
          ]
      )
