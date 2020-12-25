{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.User
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.User
  ( User (..),

    -- * Smart constructor
    mkUser,

    -- * Lenses
    uAuthenticationType,
    uArn,
    uCreatedTime,
    uEnabled,
    uFirstName,
    uLastName,
    uStatus,
    uUserName,
  )
where

import qualified Network.AWS.AppStream.Types.Arn as Types
import qualified Network.AWS.AppStream.Types.AuthenticationType as Types
import qualified Network.AWS.AppStream.Types.FirstName as Types
import qualified Network.AWS.AppStream.Types.LastName as Types
import qualified Network.AWS.AppStream.Types.String as Types
import qualified Network.AWS.AppStream.Types.Username as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a user in the user pool.
--
-- /See:/ 'mkUser' smart constructor.
data User = User'
  { -- | The authentication type for the user.
    authenticationType :: Types.AuthenticationType,
    -- | The ARN of the user.
    arn :: Core.Maybe Types.Arn,
    -- | The date and time the user was created in the user pool.
    createdTime :: Core.Maybe Core.NominalDiffTime,
    -- | Specifies whether the user in the user pool is enabled.
    enabled :: Core.Maybe Core.Bool,
    -- | The first name, or given name, of the user.
    firstName :: Core.Maybe Types.FirstName,
    -- | The last name, or surname, of the user.
    lastName :: Core.Maybe Types.LastName,
    -- | The status of the user in the user pool. The status can be one of the following:
    --
    --
    --     * UNCONFIRMED – The user is created but not confirmed.
    --
    --
    --     * CONFIRMED – The user is confirmed.
    --
    --
    --     * ARCHIVED – The user is no longer active.
    --
    --
    --     * COMPROMISED – The user is disabled because of a potential security threat.
    --
    --
    --     * UNKNOWN – The user status is not known.
    status :: Core.Maybe Types.String,
    -- | The email address of the user.
    userName :: Core.Maybe Types.Username
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'User' value with any optional fields omitted.
mkUser ::
  -- | 'authenticationType'
  Types.AuthenticationType ->
  User
mkUser authenticationType =
  User'
    { authenticationType,
      arn = Core.Nothing,
      createdTime = Core.Nothing,
      enabled = Core.Nothing,
      firstName = Core.Nothing,
      lastName = Core.Nothing,
      status = Core.Nothing,
      userName = Core.Nothing
    }

-- | The authentication type for the user.
--
-- /Note:/ Consider using 'authenticationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uAuthenticationType :: Lens.Lens' User Types.AuthenticationType
uAuthenticationType = Lens.field @"authenticationType"
{-# DEPRECATED uAuthenticationType "Use generic-lens or generic-optics with 'authenticationType' instead." #-}

-- | The ARN of the user.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uArn :: Lens.Lens' User (Core.Maybe Types.Arn)
uArn = Lens.field @"arn"
{-# DEPRECATED uArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The date and time the user was created in the user pool.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uCreatedTime :: Lens.Lens' User (Core.Maybe Core.NominalDiffTime)
uCreatedTime = Lens.field @"createdTime"
{-# DEPRECATED uCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | Specifies whether the user in the user pool is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uEnabled :: Lens.Lens' User (Core.Maybe Core.Bool)
uEnabled = Lens.field @"enabled"
{-# DEPRECATED uEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The first name, or given name, of the user.
--
-- /Note:/ Consider using 'firstName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uFirstName :: Lens.Lens' User (Core.Maybe Types.FirstName)
uFirstName = Lens.field @"firstName"
{-# DEPRECATED uFirstName "Use generic-lens or generic-optics with 'firstName' instead." #-}

-- | The last name, or surname, of the user.
--
-- /Note:/ Consider using 'lastName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uLastName :: Lens.Lens' User (Core.Maybe Types.LastName)
uLastName = Lens.field @"lastName"
{-# DEPRECATED uLastName "Use generic-lens or generic-optics with 'lastName' instead." #-}

-- | The status of the user in the user pool. The status can be one of the following:
--
--
--     * UNCONFIRMED – The user is created but not confirmed.
--
--
--     * CONFIRMED – The user is confirmed.
--
--
--     * ARCHIVED – The user is no longer active.
--
--
--     * COMPROMISED – The user is disabled because of a potential security threat.
--
--
--     * UNKNOWN – The user status is not known.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uStatus :: Lens.Lens' User (Core.Maybe Types.String)
uStatus = Lens.field @"status"
{-# DEPRECATED uStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The email address of the user.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uUserName :: Lens.Lens' User (Core.Maybe Types.Username)
uUserName = Lens.field @"userName"
{-# DEPRECATED uUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Core.FromJSON User where
  parseJSON =
    Core.withObject "User" Core.$
      \x ->
        User'
          Core.<$> (x Core..: "AuthenticationType")
          Core.<*> (x Core..:? "Arn")
          Core.<*> (x Core..:? "CreatedTime")
          Core.<*> (x Core..:? "Enabled")
          Core.<*> (x Core..:? "FirstName")
          Core.<*> (x Core..:? "LastName")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "UserName")
