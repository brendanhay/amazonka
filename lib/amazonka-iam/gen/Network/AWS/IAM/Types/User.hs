{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.User
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.User
  ( User (..),

    -- * Smart constructor
    mkUser,

    -- * Lenses
    uPasswordLastUsed,
    uPermissionsBoundary,
    uTags,
    uPath,
    uUserName,
    uUserId,
    uARN,
    uCreateDate,
  )
where

import Network.AWS.IAM.Types.AttachedPermissionsBoundary
import Network.AWS.IAM.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about an IAM user entity.
--
-- This data type is used as a response element in the following operations:
--
--     * 'CreateUser'
--
--
--     * 'GetUser'
--
--
--     * 'ListUsers'
--
--
--
-- /See:/ 'mkUser' smart constructor.
data User = User'
  { passwordLastUsed :: Lude.Maybe Lude.DateTime,
    permissionsBoundary :: Lude.Maybe AttachedPermissionsBoundary,
    tags :: Lude.Maybe [Tag],
    path :: Lude.Text,
    userName :: Lude.Text,
    userId :: Lude.Text,
    arn :: Lude.Text,
    createDate :: Lude.DateTime
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'User' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) that identifies the user. For more information about ARNs and how to use ARNs in policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
-- * 'createDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the user was created.
-- * 'passwordLastUsed' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the user's password was last used to sign in to an AWS website. For a list of AWS websites that capture a user's last sign-in time, see the <https://docs.aws.amazon.com/IAM/latest/UserGuide/credential-reports.html Credential Reports> topic in the /IAM User Guide/ . If a password is used more than once in a five-minute span, only the first use is returned in this field. If the field is null (no value), then it indicates that they never signed in with a password. This can be because:
--
--
--     * The user never had a password.
--
--
--     * A password exists but has not been used since IAM started tracking this information on October 20, 2014.
--
--
-- A null value does not mean that the user /never/ had a password. Also, if the user does not currently have a password but had one in the past, then this field contains the date and time the most recent password was used.
-- This value is returned only in the 'GetUser' and 'ListUsers' operations.
-- * 'path' - The path to the user. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
-- * 'permissionsBoundary' - The ARN of the policy used to set the permissions boundary for the user.
--
-- For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Identities > in the /IAM User Guide/ .
-- * 'tags' - A list of tags that are associated with the specified user. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
-- * 'userId' - The stable and unique string identifying the user. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
-- * 'userName' - The friendly name identifying the user.
mkUser ::
  -- | 'path'
  Lude.Text ->
  -- | 'userName'
  Lude.Text ->
  -- | 'userId'
  Lude.Text ->
  -- | 'arn'
  Lude.Text ->
  -- | 'createDate'
  Lude.DateTime ->
  User
mkUser pPath_ pUserName_ pUserId_ pARN_ pCreateDate_ =
  User'
    { passwordLastUsed = Lude.Nothing,
      permissionsBoundary = Lude.Nothing,
      tags = Lude.Nothing,
      path = pPath_,
      userName = pUserName_,
      userId = pUserId_,
      arn = pARN_,
      createDate = pCreateDate_
    }

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the user's password was last used to sign in to an AWS website. For a list of AWS websites that capture a user's last sign-in time, see the <https://docs.aws.amazon.com/IAM/latest/UserGuide/credential-reports.html Credential Reports> topic in the /IAM User Guide/ . If a password is used more than once in a five-minute span, only the first use is returned in this field. If the field is null (no value), then it indicates that they never signed in with a password. This can be because:
--
--
--     * The user never had a password.
--
--
--     * A password exists but has not been used since IAM started tracking this information on October 20, 2014.
--
--
-- A null value does not mean that the user /never/ had a password. Also, if the user does not currently have a password but had one in the past, then this field contains the date and time the most recent password was used.
-- This value is returned only in the 'GetUser' and 'ListUsers' operations.
--
-- /Note:/ Consider using 'passwordLastUsed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uPasswordLastUsed :: Lens.Lens' User (Lude.Maybe Lude.DateTime)
uPasswordLastUsed = Lens.lens (passwordLastUsed :: User -> Lude.Maybe Lude.DateTime) (\s a -> s {passwordLastUsed = a} :: User)
{-# DEPRECATED uPasswordLastUsed "Use generic-lens or generic-optics with 'passwordLastUsed' instead." #-}

-- | The ARN of the policy used to set the permissions boundary for the user.
--
-- For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Identities > in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'permissionsBoundary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uPermissionsBoundary :: Lens.Lens' User (Lude.Maybe AttachedPermissionsBoundary)
uPermissionsBoundary = Lens.lens (permissionsBoundary :: User -> Lude.Maybe AttachedPermissionsBoundary) (\s a -> s {permissionsBoundary = a} :: User)
{-# DEPRECATED uPermissionsBoundary "Use generic-lens or generic-optics with 'permissionsBoundary' instead." #-}

-- | A list of tags that are associated with the specified user. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uTags :: Lens.Lens' User (Lude.Maybe [Tag])
uTags = Lens.lens (tags :: User -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: User)
{-# DEPRECATED uTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The path to the user. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uPath :: Lens.Lens' User Lude.Text
uPath = Lens.lens (path :: User -> Lude.Text) (\s a -> s {path = a} :: User)
{-# DEPRECATED uPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The friendly name identifying the user.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uUserName :: Lens.Lens' User Lude.Text
uUserName = Lens.lens (userName :: User -> Lude.Text) (\s a -> s {userName = a} :: User)
{-# DEPRECATED uUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The stable and unique string identifying the user. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uUserId :: Lens.Lens' User Lude.Text
uUserId = Lens.lens (userId :: User -> Lude.Text) (\s a -> s {userId = a} :: User)
{-# DEPRECATED uUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The Amazon Resource Name (ARN) that identifies the user. For more information about ARNs and how to use ARNs in policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uARN :: Lens.Lens' User Lude.Text
uARN = Lens.lens (arn :: User -> Lude.Text) (\s a -> s {arn = a} :: User)
{-# DEPRECATED uARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the user was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uCreateDate :: Lens.Lens' User Lude.DateTime
uCreateDate = Lens.lens (createDate :: User -> Lude.DateTime) (\s a -> s {createDate = a} :: User)
{-# DEPRECATED uCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

instance Lude.FromXML User where
  parseXML x =
    User'
      Lude.<$> (x Lude..@? "PasswordLastUsed")
      Lude.<*> (x Lude..@? "PermissionsBoundary")
      Lude.<*> ( x Lude..@? "Tags" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@ "Path")
      Lude.<*> (x Lude..@ "UserName")
      Lude.<*> (x Lude..@ "UserId")
      Lude.<*> (x Lude..@ "Arn")
      Lude.<*> (x Lude..@ "CreateDate")
