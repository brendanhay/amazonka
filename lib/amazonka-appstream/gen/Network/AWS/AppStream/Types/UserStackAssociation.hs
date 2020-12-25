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
    usaStackName,
    usaUserName,
    usaAuthenticationType,
    usaSendEmailNotification,
  )
where

import qualified Network.AWS.AppStream.Types.AuthenticationType as Types
import qualified Network.AWS.AppStream.Types.String as Types
import qualified Network.AWS.AppStream.Types.UserName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a user in the user pool and the associated stack.
--
-- /See:/ 'mkUserStackAssociation' smart constructor.
data UserStackAssociation = UserStackAssociation'
  { -- | The name of the stack that is associated with the user.
    stackName :: Types.String,
    -- | The email address of the user who is associated with the stack.
    userName :: Types.UserName,
    -- | The authentication type for the user.
    authenticationType :: Types.AuthenticationType,
    -- | Specifies whether a welcome email is sent to a user after the user is created in the user pool.
    sendEmailNotification :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UserStackAssociation' value with any optional fields omitted.
mkUserStackAssociation ::
  -- | 'stackName'
  Types.String ->
  -- | 'userName'
  Types.UserName ->
  -- | 'authenticationType'
  Types.AuthenticationType ->
  UserStackAssociation
mkUserStackAssociation stackName userName authenticationType =
  UserStackAssociation'
    { stackName,
      userName,
      authenticationType,
      sendEmailNotification = Core.Nothing
    }

-- | The name of the stack that is associated with the user.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaStackName :: Lens.Lens' UserStackAssociation Types.String
usaStackName = Lens.field @"stackName"
{-# DEPRECATED usaStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

-- | The email address of the user who is associated with the stack.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaUserName :: Lens.Lens' UserStackAssociation Types.UserName
usaUserName = Lens.field @"userName"
{-# DEPRECATED usaUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The authentication type for the user.
--
-- /Note:/ Consider using 'authenticationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaAuthenticationType :: Lens.Lens' UserStackAssociation Types.AuthenticationType
usaAuthenticationType = Lens.field @"authenticationType"
{-# DEPRECATED usaAuthenticationType "Use generic-lens or generic-optics with 'authenticationType' instead." #-}

-- | Specifies whether a welcome email is sent to a user after the user is created in the user pool.
--
-- /Note:/ Consider using 'sendEmailNotification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaSendEmailNotification :: Lens.Lens' UserStackAssociation (Core.Maybe Core.Bool)
usaSendEmailNotification = Lens.field @"sendEmailNotification"
{-# DEPRECATED usaSendEmailNotification "Use generic-lens or generic-optics with 'sendEmailNotification' instead." #-}

instance Core.FromJSON UserStackAssociation where
  toJSON UserStackAssociation {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("StackName" Core..= stackName),
            Core.Just ("UserName" Core..= userName),
            Core.Just ("AuthenticationType" Core..= authenticationType),
            ("SendEmailNotification" Core..=) Core.<$> sendEmailNotification
          ]
      )

instance Core.FromJSON UserStackAssociation where
  parseJSON =
    Core.withObject "UserStackAssociation" Core.$
      \x ->
        UserStackAssociation'
          Core.<$> (x Core..: "StackName")
          Core.<*> (x Core..: "UserName")
          Core.<*> (x Core..: "AuthenticationType")
          Core.<*> (x Core..:? "SendEmailNotification")
