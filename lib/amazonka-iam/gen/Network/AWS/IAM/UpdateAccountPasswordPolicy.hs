{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UpdateAccountPasswordPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the password policy settings for the AWS account.
--
-- For more information about using a password policy, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_ManagingPasswordPolicies.html Managing an IAM Password Policy> in the /IAM User Guide/ .
module Network.AWS.IAM.UpdateAccountPasswordPolicy
  ( -- * Creating a request
    UpdateAccountPasswordPolicy (..),
    mkUpdateAccountPasswordPolicy,

    -- ** Request lenses
    uappAllowUsersToChangePassword,
    uappHardExpiry,
    uappMaxPasswordAge,
    uappMinimumPasswordLength,
    uappPasswordReusePrevention,
    uappRequireLowercaseCharacters,
    uappRequireNumbers,
    uappRequireSymbols,
    uappRequireUppercaseCharacters,

    -- * Destructuring the response
    UpdateAccountPasswordPolicyResponse (..),
    mkUpdateAccountPasswordPolicyResponse,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateAccountPasswordPolicy' smart constructor.
data UpdateAccountPasswordPolicy = UpdateAccountPasswordPolicy'
  { -- | Allows all IAM users in your account to use the AWS Management Console to change their own passwords. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/HowToPwdIAMUser.html Letting IAM Users Change Their Own Passwords> in the /IAM User Guide/ .
    --
    -- If you do not specify a value for this parameter, then the operation uses the default value of @false@ . The result is that IAM users in the account do not automatically have permissions to change their own password.
    allowUsersToChangePassword :: Core.Maybe Core.Bool,
    -- | Prevents IAM users from setting a new password after their password has expired. The IAM user cannot be accessed until an administrator resets the password.
    --
    -- If you do not specify a value for this parameter, then the operation uses the default value of @false@ . The result is that IAM users can change their passwords after they expire and continue to sign in as the user.
    hardExpiry :: Core.Maybe Core.Bool,
    -- | The number of days that an IAM user password is valid.
    --
    -- If you do not specify a value for this parameter, then the operation uses the default value of @0@ . The result is that IAM user passwords never expire.
    maxPasswordAge :: Core.Maybe Core.Natural,
    -- | The minimum number of characters allowed in an IAM user password.
    --
    -- If you do not specify a value for this parameter, then the operation uses the default value of @6@ .
    minimumPasswordLength :: Core.Maybe Core.Natural,
    -- | Specifies the number of previous passwords that IAM users are prevented from reusing.
    --
    -- If you do not specify a value for this parameter, then the operation uses the default value of @0@ . The result is that IAM users are not prevented from reusing previous passwords.
    passwordReusePrevention :: Core.Maybe Core.Natural,
    -- | Specifies whether IAM user passwords must contain at least one lowercase character from the ISO basic Latin alphabet (a to z).
    --
    -- If you do not specify a value for this parameter, then the operation uses the default value of @false@ . The result is that passwords do not require at least one lowercase character.
    requireLowercaseCharacters :: Core.Maybe Core.Bool,
    -- | Specifies whether IAM user passwords must contain at least one numeric character (0 to 9).
    --
    -- If you do not specify a value for this parameter, then the operation uses the default value of @false@ . The result is that passwords do not require at least one numeric character.
    requireNumbers :: Core.Maybe Core.Bool,
    -- | Specifies whether IAM user passwords must contain at least one of the following non-alphanumeric characters:
    --
    -- ! @ # $ % ^ & * ( ) _ + - = [ ] { } | '
    -- If you do not specify a value for this parameter, then the operation uses the default value of @false@ . The result is that passwords do not require at least one symbol character.
    requireSymbols :: Core.Maybe Core.Bool,
    -- | Specifies whether IAM user passwords must contain at least one uppercase character from the ISO basic Latin alphabet (A to Z).
    --
    -- If you do not specify a value for this parameter, then the operation uses the default value of @false@ . The result is that passwords do not require at least one uppercase character.
    requireUppercaseCharacters :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAccountPasswordPolicy' value with any optional fields omitted.
mkUpdateAccountPasswordPolicy ::
  UpdateAccountPasswordPolicy
mkUpdateAccountPasswordPolicy =
  UpdateAccountPasswordPolicy'
    { allowUsersToChangePassword =
        Core.Nothing,
      hardExpiry = Core.Nothing,
      maxPasswordAge = Core.Nothing,
      minimumPasswordLength = Core.Nothing,
      passwordReusePrevention = Core.Nothing,
      requireLowercaseCharacters = Core.Nothing,
      requireNumbers = Core.Nothing,
      requireSymbols = Core.Nothing,
      requireUppercaseCharacters = Core.Nothing
    }

-- | Allows all IAM users in your account to use the AWS Management Console to change their own passwords. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/HowToPwdIAMUser.html Letting IAM Users Change Their Own Passwords> in the /IAM User Guide/ .
--
-- If you do not specify a value for this parameter, then the operation uses the default value of @false@ . The result is that IAM users in the account do not automatically have permissions to change their own password.
--
-- /Note:/ Consider using 'allowUsersToChangePassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uappAllowUsersToChangePassword :: Lens.Lens' UpdateAccountPasswordPolicy (Core.Maybe Core.Bool)
uappAllowUsersToChangePassword = Lens.field @"allowUsersToChangePassword"
{-# DEPRECATED uappAllowUsersToChangePassword "Use generic-lens or generic-optics with 'allowUsersToChangePassword' instead." #-}

-- | Prevents IAM users from setting a new password after their password has expired. The IAM user cannot be accessed until an administrator resets the password.
--
-- If you do not specify a value for this parameter, then the operation uses the default value of @false@ . The result is that IAM users can change their passwords after they expire and continue to sign in as the user.
--
-- /Note:/ Consider using 'hardExpiry' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uappHardExpiry :: Lens.Lens' UpdateAccountPasswordPolicy (Core.Maybe Core.Bool)
uappHardExpiry = Lens.field @"hardExpiry"
{-# DEPRECATED uappHardExpiry "Use generic-lens or generic-optics with 'hardExpiry' instead." #-}

-- | The number of days that an IAM user password is valid.
--
-- If you do not specify a value for this parameter, then the operation uses the default value of @0@ . The result is that IAM user passwords never expire.
--
-- /Note:/ Consider using 'maxPasswordAge' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uappMaxPasswordAge :: Lens.Lens' UpdateAccountPasswordPolicy (Core.Maybe Core.Natural)
uappMaxPasswordAge = Lens.field @"maxPasswordAge"
{-# DEPRECATED uappMaxPasswordAge "Use generic-lens or generic-optics with 'maxPasswordAge' instead." #-}

-- | The minimum number of characters allowed in an IAM user password.
--
-- If you do not specify a value for this parameter, then the operation uses the default value of @6@ .
--
-- /Note:/ Consider using 'minimumPasswordLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uappMinimumPasswordLength :: Lens.Lens' UpdateAccountPasswordPolicy (Core.Maybe Core.Natural)
uappMinimumPasswordLength = Lens.field @"minimumPasswordLength"
{-# DEPRECATED uappMinimumPasswordLength "Use generic-lens or generic-optics with 'minimumPasswordLength' instead." #-}

-- | Specifies the number of previous passwords that IAM users are prevented from reusing.
--
-- If you do not specify a value for this parameter, then the operation uses the default value of @0@ . The result is that IAM users are not prevented from reusing previous passwords.
--
-- /Note:/ Consider using 'passwordReusePrevention' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uappPasswordReusePrevention :: Lens.Lens' UpdateAccountPasswordPolicy (Core.Maybe Core.Natural)
uappPasswordReusePrevention = Lens.field @"passwordReusePrevention"
{-# DEPRECATED uappPasswordReusePrevention "Use generic-lens or generic-optics with 'passwordReusePrevention' instead." #-}

-- | Specifies whether IAM user passwords must contain at least one lowercase character from the ISO basic Latin alphabet (a to z).
--
-- If you do not specify a value for this parameter, then the operation uses the default value of @false@ . The result is that passwords do not require at least one lowercase character.
--
-- /Note:/ Consider using 'requireLowercaseCharacters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uappRequireLowercaseCharacters :: Lens.Lens' UpdateAccountPasswordPolicy (Core.Maybe Core.Bool)
uappRequireLowercaseCharacters = Lens.field @"requireLowercaseCharacters"
{-# DEPRECATED uappRequireLowercaseCharacters "Use generic-lens or generic-optics with 'requireLowercaseCharacters' instead." #-}

-- | Specifies whether IAM user passwords must contain at least one numeric character (0 to 9).
--
-- If you do not specify a value for this parameter, then the operation uses the default value of @false@ . The result is that passwords do not require at least one numeric character.
--
-- /Note:/ Consider using 'requireNumbers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uappRequireNumbers :: Lens.Lens' UpdateAccountPasswordPolicy (Core.Maybe Core.Bool)
uappRequireNumbers = Lens.field @"requireNumbers"
{-# DEPRECATED uappRequireNumbers "Use generic-lens or generic-optics with 'requireNumbers' instead." #-}

-- | Specifies whether IAM user passwords must contain at least one of the following non-alphanumeric characters:
--
-- ! @ # $ % ^ & * ( ) _ + - = [ ] { } | '
-- If you do not specify a value for this parameter, then the operation uses the default value of @false@ . The result is that passwords do not require at least one symbol character.
--
-- /Note:/ Consider using 'requireSymbols' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uappRequireSymbols :: Lens.Lens' UpdateAccountPasswordPolicy (Core.Maybe Core.Bool)
uappRequireSymbols = Lens.field @"requireSymbols"
{-# DEPRECATED uappRequireSymbols "Use generic-lens or generic-optics with 'requireSymbols' instead." #-}

-- | Specifies whether IAM user passwords must contain at least one uppercase character from the ISO basic Latin alphabet (A to Z).
--
-- If you do not specify a value for this parameter, then the operation uses the default value of @false@ . The result is that passwords do not require at least one uppercase character.
--
-- /Note:/ Consider using 'requireUppercaseCharacters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uappRequireUppercaseCharacters :: Lens.Lens' UpdateAccountPasswordPolicy (Core.Maybe Core.Bool)
uappRequireUppercaseCharacters = Lens.field @"requireUppercaseCharacters"
{-# DEPRECATED uappRequireUppercaseCharacters "Use generic-lens or generic-optics with 'requireUppercaseCharacters' instead." #-}

instance Core.AWSRequest UpdateAccountPasswordPolicy where
  type
    Rs UpdateAccountPasswordPolicy =
      UpdateAccountPasswordPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "UpdateAccountPasswordPolicy")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> ( Core.toQueryValue "AllowUsersToChangePassword"
                            Core.<$> allowUsersToChangePassword
                        )
                Core.<> (Core.toQueryValue "HardExpiry" Core.<$> hardExpiry)
                Core.<> (Core.toQueryValue "MaxPasswordAge" Core.<$> maxPasswordAge)
                Core.<> ( Core.toQueryValue "MinimumPasswordLength"
                            Core.<$> minimumPasswordLength
                        )
                Core.<> ( Core.toQueryValue "PasswordReusePrevention"
                            Core.<$> passwordReusePrevention
                        )
                Core.<> ( Core.toQueryValue "RequireLowercaseCharacters"
                            Core.<$> requireLowercaseCharacters
                        )
                Core.<> (Core.toQueryValue "RequireNumbers" Core.<$> requireNumbers)
                Core.<> (Core.toQueryValue "RequireSymbols" Core.<$> requireSymbols)
                Core.<> ( Core.toQueryValue "RequireUppercaseCharacters"
                            Core.<$> requireUppercaseCharacters
                        )
            )
      }
  response =
    Response.receiveNull UpdateAccountPasswordPolicyResponse'

-- | /See:/ 'mkUpdateAccountPasswordPolicyResponse' smart constructor.
data UpdateAccountPasswordPolicyResponse = UpdateAccountPasswordPolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAccountPasswordPolicyResponse' value with any optional fields omitted.
mkUpdateAccountPasswordPolicyResponse ::
  UpdateAccountPasswordPolicyResponse
mkUpdateAccountPasswordPolicyResponse =
  UpdateAccountPasswordPolicyResponse'
