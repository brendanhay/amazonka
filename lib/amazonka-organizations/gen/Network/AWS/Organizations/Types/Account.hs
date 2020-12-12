{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.Account
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.Account
  ( Account (..),

    -- * Smart constructor
    mkAccount,

    -- * Lenses
    aStatus,
    aJoinedMethod,
    aEmail,
    aARN,
    aJoinedTimestamp,
    aName,
    aId,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types.AccountJoinedMethod
import Network.AWS.Organizations.Types.AccountStatus
import qualified Network.AWS.Prelude as Lude

-- | Contains information about an AWS account that is a member of an organization.
--
-- /See:/ 'mkAccount' smart constructor.
data Account = Account'
  { status :: Lude.Maybe AccountStatus,
    joinedMethod :: Lude.Maybe AccountJoinedMethod,
    email :: Lude.Maybe (Lude.Sensitive Lude.Text),
    arn :: Lude.Maybe Lude.Text,
    joinedTimestamp :: Lude.Maybe Lude.Timestamp,
    name :: Lude.Maybe (Lude.Sensitive Lude.Text),
    id :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Account' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the account.
--
-- For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
-- * 'email' - The email address associated with the AWS account.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for this parameter is a string of characters that represents a standard internet email address.
-- * 'id' - The unique identifier (ID) of the account.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
-- * 'joinedMethod' - The method by which the account joined the organization.
-- * 'joinedTimestamp' - The date the account became a part of the organization.
-- * 'name' - The friendly name of the account.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
-- * 'status' - The status of the account in the organization.
mkAccount ::
  Account
mkAccount =
  Account'
    { status = Lude.Nothing,
      joinedMethod = Lude.Nothing,
      email = Lude.Nothing,
      arn = Lude.Nothing,
      joinedTimestamp = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing
    }

-- | The status of the account in the organization.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aStatus :: Lens.Lens' Account (Lude.Maybe AccountStatus)
aStatus = Lens.lens (status :: Account -> Lude.Maybe AccountStatus) (\s a -> s {status = a} :: Account)
{-# DEPRECATED aStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The method by which the account joined the organization.
--
-- /Note:/ Consider using 'joinedMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aJoinedMethod :: Lens.Lens' Account (Lude.Maybe AccountJoinedMethod)
aJoinedMethod = Lens.lens (joinedMethod :: Account -> Lude.Maybe AccountJoinedMethod) (\s a -> s {joinedMethod = a} :: Account)
{-# DEPRECATED aJoinedMethod "Use generic-lens or generic-optics with 'joinedMethod' instead." #-}

-- | The email address associated with the AWS account.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for this parameter is a string of characters that represents a standard internet email address.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aEmail :: Lens.Lens' Account (Lude.Maybe (Lude.Sensitive Lude.Text))
aEmail = Lens.lens (email :: Account -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {email = a} :: Account)
{-# DEPRECATED aEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | The Amazon Resource Name (ARN) of the account.
--
-- For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aARN :: Lens.Lens' Account (Lude.Maybe Lude.Text)
aARN = Lens.lens (arn :: Account -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Account)
{-# DEPRECATED aARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The date the account became a part of the organization.
--
-- /Note:/ Consider using 'joinedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aJoinedTimestamp :: Lens.Lens' Account (Lude.Maybe Lude.Timestamp)
aJoinedTimestamp = Lens.lens (joinedTimestamp :: Account -> Lude.Maybe Lude.Timestamp) (\s a -> s {joinedTimestamp = a} :: Account)
{-# DEPRECATED aJoinedTimestamp "Use generic-lens or generic-optics with 'joinedTimestamp' instead." #-}

-- | The friendly name of the account.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aName :: Lens.Lens' Account (Lude.Maybe (Lude.Sensitive Lude.Text))
aName = Lens.lens (name :: Account -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {name = a} :: Account)
{-# DEPRECATED aName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The unique identifier (ID) of the account.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aId :: Lens.Lens' Account (Lude.Maybe Lude.Text)
aId = Lens.lens (id :: Account -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Account)
{-# DEPRECATED aId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON Account where
  parseJSON =
    Lude.withObject
      "Account"
      ( \x ->
          Account'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "JoinedMethod")
            Lude.<*> (x Lude..:? "Email")
            Lude.<*> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "JoinedTimestamp")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
      )
