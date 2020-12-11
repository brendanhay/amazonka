-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.UserProfileSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStar.Types.UserProfileSummary
  ( UserProfileSummary (..),

    -- * Smart constructor
    mkUserProfileSummary,

    -- * Lenses
    upsSshPublicKey,
    upsUserARN,
    upsEmailAddress,
    upsDisplayName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a user's profile in AWS CodeStar.
--
-- /See:/ 'mkUserProfileSummary' smart constructor.
data UserProfileSummary = UserProfileSummary'
  { sshPublicKey ::
      Lude.Maybe Lude.Text,
    userARN :: Lude.Maybe Lude.Text,
    emailAddress :: Lude.Maybe (Lude.Sensitive Lude.Text),
    displayName :: Lude.Maybe (Lude.Sensitive Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UserProfileSummary' with the minimum fields required to make a request.
--
-- * 'displayName' - The display name of a user in AWS CodeStar. For example, this could be set to both first and last name ("Mary Major") or a single name ("Mary"). The display name is also used to generate the initial icon associated with the user in AWS CodeStar projects. If spaces are included in the display name, the first character that appears after the space will be used as the second character in the user initial icon. The initial icon displays a maximum of two characters, so a display name with more than one space (for example "Mary Jane Major") would generate an initial icon using the first character and the first character after the space ("MJ", not "MM").
-- * 'emailAddress' - The email address associated with the user.
-- * 'sshPublicKey' - The SSH public key associated with the user in AWS CodeStar. If a project owner allows the user remote access to project resources, this public key will be used along with the user's private key for SSH access.
-- * 'userARN' - The Amazon Resource Name (ARN) of the user in IAM.
mkUserProfileSummary ::
  UserProfileSummary
mkUserProfileSummary =
  UserProfileSummary'
    { sshPublicKey = Lude.Nothing,
      userARN = Lude.Nothing,
      emailAddress = Lude.Nothing,
      displayName = Lude.Nothing
    }

-- | The SSH public key associated with the user in AWS CodeStar. If a project owner allows the user remote access to project resources, this public key will be used along with the user's private key for SSH access.
--
-- /Note:/ Consider using 'sshPublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upsSshPublicKey :: Lens.Lens' UserProfileSummary (Lude.Maybe Lude.Text)
upsSshPublicKey = Lens.lens (sshPublicKey :: UserProfileSummary -> Lude.Maybe Lude.Text) (\s a -> s {sshPublicKey = a} :: UserProfileSummary)
{-# DEPRECATED upsSshPublicKey "Use generic-lens or generic-optics with 'sshPublicKey' instead." #-}

-- | The Amazon Resource Name (ARN) of the user in IAM.
--
-- /Note:/ Consider using 'userARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upsUserARN :: Lens.Lens' UserProfileSummary (Lude.Maybe Lude.Text)
upsUserARN = Lens.lens (userARN :: UserProfileSummary -> Lude.Maybe Lude.Text) (\s a -> s {userARN = a} :: UserProfileSummary)
{-# DEPRECATED upsUserARN "Use generic-lens or generic-optics with 'userARN' instead." #-}

-- | The email address associated with the user.
--
-- /Note:/ Consider using 'emailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upsEmailAddress :: Lens.Lens' UserProfileSummary (Lude.Maybe (Lude.Sensitive Lude.Text))
upsEmailAddress = Lens.lens (emailAddress :: UserProfileSummary -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {emailAddress = a} :: UserProfileSummary)
{-# DEPRECATED upsEmailAddress "Use generic-lens or generic-optics with 'emailAddress' instead." #-}

-- | The display name of a user in AWS CodeStar. For example, this could be set to both first and last name ("Mary Major") or a single name ("Mary"). The display name is also used to generate the initial icon associated with the user in AWS CodeStar projects. If spaces are included in the display name, the first character that appears after the space will be used as the second character in the user initial icon. The initial icon displays a maximum of two characters, so a display name with more than one space (for example "Mary Jane Major") would generate an initial icon using the first character and the first character after the space ("MJ", not "MM").
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upsDisplayName :: Lens.Lens' UserProfileSummary (Lude.Maybe (Lude.Sensitive Lude.Text))
upsDisplayName = Lens.lens (displayName :: UserProfileSummary -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {displayName = a} :: UserProfileSummary)
{-# DEPRECATED upsDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

instance Lude.FromJSON UserProfileSummary where
  parseJSON =
    Lude.withObject
      "UserProfileSummary"
      ( \x ->
          UserProfileSummary'
            Lude.<$> (x Lude..:? "sshPublicKey")
            Lude.<*> (x Lude..:? "userArn")
            Lude.<*> (x Lude..:? "emailAddress")
            Lude.<*> (x Lude..:? "displayName")
      )
