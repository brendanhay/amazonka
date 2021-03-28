{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.UserType
  ( UserType (..)
  -- * Smart constructor
  , mkUserType
  -- * Lenses
  , utAttributes
  , utEnabled
  , utMFAOptions
  , utUserCreateDate
  , utUserLastModifiedDate
  , utUserStatus
  , utUsername
  ) where

import qualified Network.AWS.CognitoIdentityProvider.Types.AttributeType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.MFAOptionType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.UserStatusType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.UsernameType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The user type.
--
-- /See:/ 'mkUserType' smart constructor.
data UserType = UserType'
  { attributes :: Core.Maybe [Types.AttributeType]
    -- ^ A container with information about the user type attributes.
  , enabled :: Core.Maybe Core.Bool
    -- ^ Specifies whether the user is enabled.
  , mFAOptions :: Core.Maybe [Types.MFAOptionType]
    -- ^ The MFA options for the user.
  , userCreateDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The creation date of the user.
  , userLastModifiedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The last modified date of the user.
  , userStatus :: Core.Maybe Types.UserStatusType
    -- ^ The user status. Can be one of the following:
--
--
--     * UNCONFIRMED - User has been created but not confirmed.
--
--
--     * CONFIRMED - User has been confirmed.
--
--
--     * ARCHIVED - User is no longer active.
--
--
--     * COMPROMISED - User is disabled due to a potential security threat.
--
--
--     * UNKNOWN - User status is not known.
--
--
--     * RESET_REQUIRED - User is confirmed, but the user must request a code and reset his or her password before he or she can sign in.
--
--
--     * FORCE_CHANGE_PASSWORD - The user is confirmed and the user can sign in using a temporary password, but on first sign-in, the user must change his or her password to a new value before doing anything else. 
--
--
  , username :: Core.Maybe Types.UsernameType
    -- ^ The user name of the user you wish to describe.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UserType' value with any optional fields omitted.
mkUserType
    :: UserType
mkUserType
  = UserType'{attributes = Core.Nothing, enabled = Core.Nothing,
              mFAOptions = Core.Nothing, userCreateDate = Core.Nothing,
              userLastModifiedDate = Core.Nothing, userStatus = Core.Nothing,
              username = Core.Nothing}

-- | A container with information about the user type attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utAttributes :: Lens.Lens' UserType (Core.Maybe [Types.AttributeType])
utAttributes = Lens.field @"attributes"
{-# INLINEABLE utAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | Specifies whether the user is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utEnabled :: Lens.Lens' UserType (Core.Maybe Core.Bool)
utEnabled = Lens.field @"enabled"
{-# INLINEABLE utEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | The MFA options for the user.
--
-- /Note:/ Consider using 'mFAOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utMFAOptions :: Lens.Lens' UserType (Core.Maybe [Types.MFAOptionType])
utMFAOptions = Lens.field @"mFAOptions"
{-# INLINEABLE utMFAOptions #-}
{-# DEPRECATED mFAOptions "Use generic-lens or generic-optics with 'mFAOptions' instead"  #-}

-- | The creation date of the user.
--
-- /Note:/ Consider using 'userCreateDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utUserCreateDate :: Lens.Lens' UserType (Core.Maybe Core.NominalDiffTime)
utUserCreateDate = Lens.field @"userCreateDate"
{-# INLINEABLE utUserCreateDate #-}
{-# DEPRECATED userCreateDate "Use generic-lens or generic-optics with 'userCreateDate' instead"  #-}

-- | The last modified date of the user.
--
-- /Note:/ Consider using 'userLastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utUserLastModifiedDate :: Lens.Lens' UserType (Core.Maybe Core.NominalDiffTime)
utUserLastModifiedDate = Lens.field @"userLastModifiedDate"
{-# INLINEABLE utUserLastModifiedDate #-}
{-# DEPRECATED userLastModifiedDate "Use generic-lens or generic-optics with 'userLastModifiedDate' instead"  #-}

-- | The user status. Can be one of the following:
--
--
--     * UNCONFIRMED - User has been created but not confirmed.
--
--
--     * CONFIRMED - User has been confirmed.
--
--
--     * ARCHIVED - User is no longer active.
--
--
--     * COMPROMISED - User is disabled due to a potential security threat.
--
--
--     * UNKNOWN - User status is not known.
--
--
--     * RESET_REQUIRED - User is confirmed, but the user must request a code and reset his or her password before he or she can sign in.
--
--
--     * FORCE_CHANGE_PASSWORD - The user is confirmed and the user can sign in using a temporary password, but on first sign-in, the user must change his or her password to a new value before doing anything else. 
--
--
--
-- /Note:/ Consider using 'userStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utUserStatus :: Lens.Lens' UserType (Core.Maybe Types.UserStatusType)
utUserStatus = Lens.field @"userStatus"
{-# INLINEABLE utUserStatus #-}
{-# DEPRECATED userStatus "Use generic-lens or generic-optics with 'userStatus' instead"  #-}

-- | The user name of the user you wish to describe.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utUsername :: Lens.Lens' UserType (Core.Maybe Types.UsernameType)
utUsername = Lens.field @"username"
{-# INLINEABLE utUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

instance Core.FromJSON UserType where
        parseJSON
          = Core.withObject "UserType" Core.$
              \ x ->
                UserType' Core.<$>
                  (x Core..:? "Attributes") Core.<*> x Core..:? "Enabled" Core.<*>
                    x Core..:? "MFAOptions"
                    Core.<*> x Core..:? "UserCreateDate"
                    Core.<*> x Core..:? "UserLastModifiedDate"
                    Core.<*> x Core..:? "UserStatus"
                    Core.<*> x Core..:? "Username"
