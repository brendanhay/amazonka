{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.UserStackAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.UserStackAssociation
  ( UserStackAssociation (..),

    -- * Smart constructor
    mkUserStackAssociation,

    -- * Lenses
    usaSendEmailNotification,
    usaStackName,
    usaUserName,
    usaAuthenticationType,
  )
where

import Network.AWS.AppStream.Types.AuthenticationType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a user in the user pool and the associated stack.
--
-- /See:/ 'mkUserStackAssociation' smart constructor.
data UserStackAssociation = UserStackAssociation'
  { sendEmailNotification ::
      Lude.Maybe Lude.Bool,
    stackName :: Lude.Text,
    userName :: Lude.Sensitive Lude.Text,
    authenticationType :: AuthenticationType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UserStackAssociation' with the minimum fields required to make a request.
--
-- * 'authenticationType' - The authentication type for the user.
-- * 'sendEmailNotification' - Specifies whether a welcome email is sent to a user after the user is created in the user pool.
-- * 'stackName' - The name of the stack that is associated with the user.
-- * 'userName' - The email address of the user who is associated with the stack.
mkUserStackAssociation ::
  -- | 'stackName'
  Lude.Text ->
  -- | 'userName'
  Lude.Sensitive Lude.Text ->
  -- | 'authenticationType'
  AuthenticationType ->
  UserStackAssociation
mkUserStackAssociation pStackName_ pUserName_ pAuthenticationType_ =
  UserStackAssociation'
    { sendEmailNotification = Lude.Nothing,
      stackName = pStackName_,
      userName = pUserName_,
      authenticationType = pAuthenticationType_
    }

-- | Specifies whether a welcome email is sent to a user after the user is created in the user pool.
--
-- /Note:/ Consider using 'sendEmailNotification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaSendEmailNotification :: Lens.Lens' UserStackAssociation (Lude.Maybe Lude.Bool)
usaSendEmailNotification = Lens.lens (sendEmailNotification :: UserStackAssociation -> Lude.Maybe Lude.Bool) (\s a -> s {sendEmailNotification = a} :: UserStackAssociation)
{-# DEPRECATED usaSendEmailNotification "Use generic-lens or generic-optics with 'sendEmailNotification' instead." #-}

-- | The name of the stack that is associated with the user.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaStackName :: Lens.Lens' UserStackAssociation Lude.Text
usaStackName = Lens.lens (stackName :: UserStackAssociation -> Lude.Text) (\s a -> s {stackName = a} :: UserStackAssociation)
{-# DEPRECATED usaStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

-- | The email address of the user who is associated with the stack.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaUserName :: Lens.Lens' UserStackAssociation (Lude.Sensitive Lude.Text)
usaUserName = Lens.lens (userName :: UserStackAssociation -> Lude.Sensitive Lude.Text) (\s a -> s {userName = a} :: UserStackAssociation)
{-# DEPRECATED usaUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The authentication type for the user.
--
-- /Note:/ Consider using 'authenticationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaAuthenticationType :: Lens.Lens' UserStackAssociation AuthenticationType
usaAuthenticationType = Lens.lens (authenticationType :: UserStackAssociation -> AuthenticationType) (\s a -> s {authenticationType = a} :: UserStackAssociation)
{-# DEPRECATED usaAuthenticationType "Use generic-lens or generic-optics with 'authenticationType' instead." #-}

instance Lude.FromJSON UserStackAssociation where
  parseJSON =
    Lude.withObject
      "UserStackAssociation"
      ( \x ->
          UserStackAssociation'
            Lude.<$> (x Lude..:? "SendEmailNotification")
            Lude.<*> (x Lude..: "StackName")
            Lude.<*> (x Lude..: "UserName")
            Lude.<*> (x Lude..: "AuthenticationType")
      )

instance Lude.ToJSON UserStackAssociation where
  toJSON UserStackAssociation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SendEmailNotification" Lude..=) Lude.<$> sendEmailNotification,
            Lude.Just ("StackName" Lude..= stackName),
            Lude.Just ("UserName" Lude..= userName),
            Lude.Just ("AuthenticationType" Lude..= authenticationType)
          ]
      )
