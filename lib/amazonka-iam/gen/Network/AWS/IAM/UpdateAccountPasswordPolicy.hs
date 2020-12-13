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
    uappMinimumPasswordLength,
    uappRequireNumbers,
    uappPasswordReusePrevention,
    uappRequireLowercaseCharacters,
    uappMaxPasswordAge,
    uappHardExpiry,
    uappRequireSymbols,
    uappRequireUppercaseCharacters,
    uappAllowUsersToChangePassword,

    -- * Destructuring the response
    UpdateAccountPasswordPolicyResponse (..),
    mkUpdateAccountPasswordPolicyResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateAccountPasswordPolicy' smart constructor.
data UpdateAccountPasswordPolicy = UpdateAccountPasswordPolicy'
  { -- | The minimum number of characters allowed in an IAM user password.
    --
    -- If you do not specify a value for this parameter, then the operation uses the default value of @6@ .
    minimumPasswordLength :: Lude.Maybe Lude.Natural,
    -- | Specifies whether IAM user passwords must contain at least one numeric character (0 to 9).
    --
    -- If you do not specify a value for this parameter, then the operation uses the default value of @false@ . The result is that passwords do not require at least one numeric character.
    requireNumbers :: Lude.Maybe Lude.Bool,
    -- | Specifies the number of previous passwords that IAM users are prevented from reusing.
    --
    -- If you do not specify a value for this parameter, then the operation uses the default value of @0@ . The result is that IAM users are not prevented from reusing previous passwords.
    passwordReusePrevention :: Lude.Maybe Lude.Natural,
    -- | Specifies whether IAM user passwords must contain at least one lowercase character from the ISO basic Latin alphabet (a to z).
    --
    -- If you do not specify a value for this parameter, then the operation uses the default value of @false@ . The result is that passwords do not require at least one lowercase character.
    requireLowercaseCharacters :: Lude.Maybe Lude.Bool,
    -- | The number of days that an IAM user password is valid.
    --
    -- If you do not specify a value for this parameter, then the operation uses the default value of @0@ . The result is that IAM user passwords never expire.
    maxPasswordAge :: Lude.Maybe Lude.Natural,
    -- | Prevents IAM users from setting a new password after their password has expired. The IAM user cannot be accessed until an administrator resets the password.
    --
    -- If you do not specify a value for this parameter, then the operation uses the default value of @false@ . The result is that IAM users can change their passwords after they expire and continue to sign in as the user.
    hardExpiry :: Lude.Maybe Lude.Bool,
    -- | Specifies whether IAM user passwords must contain at least one of the following non-alphanumeric characters:
    --
    -- ! @ # $ % ^ & * ( ) _ + - = [ ] { } | '
    -- If you do not specify a value for this parameter, then the operation uses the default value of @false@ . The result is that passwords do not require at least one symbol character.
    requireSymbols :: Lude.Maybe Lude.Bool,
    -- | Specifies whether IAM user passwords must contain at least one uppercase character from the ISO basic Latin alphabet (A to Z).
    --
    -- If you do not specify a value for this parameter, then the operation uses the default value of @false@ . The result is that passwords do not require at least one uppercase character.
    requireUppercaseCharacters :: Lude.Maybe Lude.Bool,
    -- | Allows all IAM users in your account to use the AWS Management Console to change their own passwords. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/HowToPwdIAMUser.html Letting IAM Users Change Their Own Passwords> in the /IAM User Guide/ .
    --
    -- If you do not specify a value for this parameter, then the operation uses the default value of @false@ . The result is that IAM users in the account do not automatically have permissions to change their own password.
    allowUsersToChangePassword :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAccountPasswordPolicy' with the minimum fields required to make a request.
--
-- * 'minimumPasswordLength' - The minimum number of characters allowed in an IAM user password.
--
-- If you do not specify a value for this parameter, then the operation uses the default value of @6@ .
-- * 'requireNumbers' - Specifies whether IAM user passwords must contain at least one numeric character (0 to 9).
--
-- If you do not specify a value for this parameter, then the operation uses the default value of @false@ . The result is that passwords do not require at least one numeric character.
-- * 'passwordReusePrevention' - Specifies the number of previous passwords that IAM users are prevented from reusing.
--
-- If you do not specify a value for this parameter, then the operation uses the default value of @0@ . The result is that IAM users are not prevented from reusing previous passwords.
-- * 'requireLowercaseCharacters' - Specifies whether IAM user passwords must contain at least one lowercase character from the ISO basic Latin alphabet (a to z).
--
-- If you do not specify a value for this parameter, then the operation uses the default value of @false@ . The result is that passwords do not require at least one lowercase character.
-- * 'maxPasswordAge' - The number of days that an IAM user password is valid.
--
-- If you do not specify a value for this parameter, then the operation uses the default value of @0@ . The result is that IAM user passwords never expire.
-- * 'hardExpiry' - Prevents IAM users from setting a new password after their password has expired. The IAM user cannot be accessed until an administrator resets the password.
--
-- If you do not specify a value for this parameter, then the operation uses the default value of @false@ . The result is that IAM users can change their passwords after they expire and continue to sign in as the user.
-- * 'requireSymbols' - Specifies whether IAM user passwords must contain at least one of the following non-alphanumeric characters:
--
-- ! @ # $ % ^ & * ( ) _ + - = [ ] { } | '
-- If you do not specify a value for this parameter, then the operation uses the default value of @false@ . The result is that passwords do not require at least one symbol character.
-- * 'requireUppercaseCharacters' - Specifies whether IAM user passwords must contain at least one uppercase character from the ISO basic Latin alphabet (A to Z).
--
-- If you do not specify a value for this parameter, then the operation uses the default value of @false@ . The result is that passwords do not require at least one uppercase character.
-- * 'allowUsersToChangePassword' - Allows all IAM users in your account to use the AWS Management Console to change their own passwords. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/HowToPwdIAMUser.html Letting IAM Users Change Their Own Passwords> in the /IAM User Guide/ .
--
-- If you do not specify a value for this parameter, then the operation uses the default value of @false@ . The result is that IAM users in the account do not automatically have permissions to change their own password.
mkUpdateAccountPasswordPolicy ::
  UpdateAccountPasswordPolicy
mkUpdateAccountPasswordPolicy =
  UpdateAccountPasswordPolicy'
    { minimumPasswordLength =
        Lude.Nothing,
      requireNumbers = Lude.Nothing,
      passwordReusePrevention = Lude.Nothing,
      requireLowercaseCharacters = Lude.Nothing,
      maxPasswordAge = Lude.Nothing,
      hardExpiry = Lude.Nothing,
      requireSymbols = Lude.Nothing,
      requireUppercaseCharacters = Lude.Nothing,
      allowUsersToChangePassword = Lude.Nothing
    }

-- | The minimum number of characters allowed in an IAM user password.
--
-- If you do not specify a value for this parameter, then the operation uses the default value of @6@ .
--
-- /Note:/ Consider using 'minimumPasswordLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uappMinimumPasswordLength :: Lens.Lens' UpdateAccountPasswordPolicy (Lude.Maybe Lude.Natural)
uappMinimumPasswordLength = Lens.lens (minimumPasswordLength :: UpdateAccountPasswordPolicy -> Lude.Maybe Lude.Natural) (\s a -> s {minimumPasswordLength = a} :: UpdateAccountPasswordPolicy)
{-# DEPRECATED uappMinimumPasswordLength "Use generic-lens or generic-optics with 'minimumPasswordLength' instead." #-}

-- | Specifies whether IAM user passwords must contain at least one numeric character (0 to 9).
--
-- If you do not specify a value for this parameter, then the operation uses the default value of @false@ . The result is that passwords do not require at least one numeric character.
--
-- /Note:/ Consider using 'requireNumbers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uappRequireNumbers :: Lens.Lens' UpdateAccountPasswordPolicy (Lude.Maybe Lude.Bool)
uappRequireNumbers = Lens.lens (requireNumbers :: UpdateAccountPasswordPolicy -> Lude.Maybe Lude.Bool) (\s a -> s {requireNumbers = a} :: UpdateAccountPasswordPolicy)
{-# DEPRECATED uappRequireNumbers "Use generic-lens or generic-optics with 'requireNumbers' instead." #-}

-- | Specifies the number of previous passwords that IAM users are prevented from reusing.
--
-- If you do not specify a value for this parameter, then the operation uses the default value of @0@ . The result is that IAM users are not prevented from reusing previous passwords.
--
-- /Note:/ Consider using 'passwordReusePrevention' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uappPasswordReusePrevention :: Lens.Lens' UpdateAccountPasswordPolicy (Lude.Maybe Lude.Natural)
uappPasswordReusePrevention = Lens.lens (passwordReusePrevention :: UpdateAccountPasswordPolicy -> Lude.Maybe Lude.Natural) (\s a -> s {passwordReusePrevention = a} :: UpdateAccountPasswordPolicy)
{-# DEPRECATED uappPasswordReusePrevention "Use generic-lens or generic-optics with 'passwordReusePrevention' instead." #-}

-- | Specifies whether IAM user passwords must contain at least one lowercase character from the ISO basic Latin alphabet (a to z).
--
-- If you do not specify a value for this parameter, then the operation uses the default value of @false@ . The result is that passwords do not require at least one lowercase character.
--
-- /Note:/ Consider using 'requireLowercaseCharacters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uappRequireLowercaseCharacters :: Lens.Lens' UpdateAccountPasswordPolicy (Lude.Maybe Lude.Bool)
uappRequireLowercaseCharacters = Lens.lens (requireLowercaseCharacters :: UpdateAccountPasswordPolicy -> Lude.Maybe Lude.Bool) (\s a -> s {requireLowercaseCharacters = a} :: UpdateAccountPasswordPolicy)
{-# DEPRECATED uappRequireLowercaseCharacters "Use generic-lens or generic-optics with 'requireLowercaseCharacters' instead." #-}

-- | The number of days that an IAM user password is valid.
--
-- If you do not specify a value for this parameter, then the operation uses the default value of @0@ . The result is that IAM user passwords never expire.
--
-- /Note:/ Consider using 'maxPasswordAge' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uappMaxPasswordAge :: Lens.Lens' UpdateAccountPasswordPolicy (Lude.Maybe Lude.Natural)
uappMaxPasswordAge = Lens.lens (maxPasswordAge :: UpdateAccountPasswordPolicy -> Lude.Maybe Lude.Natural) (\s a -> s {maxPasswordAge = a} :: UpdateAccountPasswordPolicy)
{-# DEPRECATED uappMaxPasswordAge "Use generic-lens or generic-optics with 'maxPasswordAge' instead." #-}

-- | Prevents IAM users from setting a new password after their password has expired. The IAM user cannot be accessed until an administrator resets the password.
--
-- If you do not specify a value for this parameter, then the operation uses the default value of @false@ . The result is that IAM users can change their passwords after they expire and continue to sign in as the user.
--
-- /Note:/ Consider using 'hardExpiry' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uappHardExpiry :: Lens.Lens' UpdateAccountPasswordPolicy (Lude.Maybe Lude.Bool)
uappHardExpiry = Lens.lens (hardExpiry :: UpdateAccountPasswordPolicy -> Lude.Maybe Lude.Bool) (\s a -> s {hardExpiry = a} :: UpdateAccountPasswordPolicy)
{-# DEPRECATED uappHardExpiry "Use generic-lens or generic-optics with 'hardExpiry' instead." #-}

-- | Specifies whether IAM user passwords must contain at least one of the following non-alphanumeric characters:
--
-- ! @ # $ % ^ & * ( ) _ + - = [ ] { } | '
-- If you do not specify a value for this parameter, then the operation uses the default value of @false@ . The result is that passwords do not require at least one symbol character.
--
-- /Note:/ Consider using 'requireSymbols' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uappRequireSymbols :: Lens.Lens' UpdateAccountPasswordPolicy (Lude.Maybe Lude.Bool)
uappRequireSymbols = Lens.lens (requireSymbols :: UpdateAccountPasswordPolicy -> Lude.Maybe Lude.Bool) (\s a -> s {requireSymbols = a} :: UpdateAccountPasswordPolicy)
{-# DEPRECATED uappRequireSymbols "Use generic-lens or generic-optics with 'requireSymbols' instead." #-}

-- | Specifies whether IAM user passwords must contain at least one uppercase character from the ISO basic Latin alphabet (A to Z).
--
-- If you do not specify a value for this parameter, then the operation uses the default value of @false@ . The result is that passwords do not require at least one uppercase character.
--
-- /Note:/ Consider using 'requireUppercaseCharacters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uappRequireUppercaseCharacters :: Lens.Lens' UpdateAccountPasswordPolicy (Lude.Maybe Lude.Bool)
uappRequireUppercaseCharacters = Lens.lens (requireUppercaseCharacters :: UpdateAccountPasswordPolicy -> Lude.Maybe Lude.Bool) (\s a -> s {requireUppercaseCharacters = a} :: UpdateAccountPasswordPolicy)
{-# DEPRECATED uappRequireUppercaseCharacters "Use generic-lens or generic-optics with 'requireUppercaseCharacters' instead." #-}

-- | Allows all IAM users in your account to use the AWS Management Console to change their own passwords. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/HowToPwdIAMUser.html Letting IAM Users Change Their Own Passwords> in the /IAM User Guide/ .
--
-- If you do not specify a value for this parameter, then the operation uses the default value of @false@ . The result is that IAM users in the account do not automatically have permissions to change their own password.
--
-- /Note:/ Consider using 'allowUsersToChangePassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uappAllowUsersToChangePassword :: Lens.Lens' UpdateAccountPasswordPolicy (Lude.Maybe Lude.Bool)
uappAllowUsersToChangePassword = Lens.lens (allowUsersToChangePassword :: UpdateAccountPasswordPolicy -> Lude.Maybe Lude.Bool) (\s a -> s {allowUsersToChangePassword = a} :: UpdateAccountPasswordPolicy)
{-# DEPRECATED uappAllowUsersToChangePassword "Use generic-lens or generic-optics with 'allowUsersToChangePassword' instead." #-}

instance Lude.AWSRequest UpdateAccountPasswordPolicy where
  type
    Rs UpdateAccountPasswordPolicy =
      UpdateAccountPasswordPolicyResponse
  request = Req.postQuery iamService
  response = Res.receiveNull UpdateAccountPasswordPolicyResponse'

instance Lude.ToHeaders UpdateAccountPasswordPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateAccountPasswordPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateAccountPasswordPolicy where
  toQuery UpdateAccountPasswordPolicy' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("UpdateAccountPasswordPolicy" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "MinimumPasswordLength" Lude.=: minimumPasswordLength,
        "RequireNumbers" Lude.=: requireNumbers,
        "PasswordReusePrevention" Lude.=: passwordReusePrevention,
        "RequireLowercaseCharacters" Lude.=: requireLowercaseCharacters,
        "MaxPasswordAge" Lude.=: maxPasswordAge,
        "HardExpiry" Lude.=: hardExpiry,
        "RequireSymbols" Lude.=: requireSymbols,
        "RequireUppercaseCharacters" Lude.=: requireUppercaseCharacters,
        "AllowUsersToChangePassword" Lude.=: allowUsersToChangePassword
      ]

-- | /See:/ 'mkUpdateAccountPasswordPolicyResponse' smart constructor.
data UpdateAccountPasswordPolicyResponse = UpdateAccountPasswordPolicyResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAccountPasswordPolicyResponse' with the minimum fields required to make a request.
mkUpdateAccountPasswordPolicyResponse ::
  UpdateAccountPasswordPolicyResponse
mkUpdateAccountPasswordPolicyResponse =
  UpdateAccountPasswordPolicyResponse'
