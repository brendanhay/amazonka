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
    uPath,
    uUserName,
    uUserId,
    uArn,
    uCreateDate,
    uPasswordLastUsed,
    uPermissionsBoundary,
    uTags,
  )
where

import qualified Network.AWS.IAM.Types.Arn as Types
import qualified Network.AWS.IAM.Types.AttachedPermissionsBoundary as Types
import qualified Network.AWS.IAM.Types.IdType as Types
import qualified Network.AWS.IAM.Types.Path as Types
import qualified Network.AWS.IAM.Types.Tag as Types
import qualified Network.AWS.IAM.Types.UserName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

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
  { -- | The path to the user. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
    path :: Types.Path,
    -- | The friendly name identifying the user.
    userName :: Types.UserName,
    -- | The stable and unique string identifying the user. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
    userId :: Types.IdType,
    -- | The Amazon Resource Name (ARN) that identifies the user. For more information about ARNs and how to use ARNs in policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
    arn :: Types.Arn,
    -- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the user was created.
    createDate :: Core.UTCTime,
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
    passwordLastUsed :: Core.Maybe Core.UTCTime,
    -- | The ARN of the policy used to set the permissions boundary for the user.
    --
    -- For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Identities > in the /IAM User Guide/ .
    permissionsBoundary :: Core.Maybe Types.AttachedPermissionsBoundary,
    -- | A list of tags that are associated with the specified user. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'User' value with any optional fields omitted.
mkUser ::
  -- | 'path'
  Types.Path ->
  -- | 'userName'
  Types.UserName ->
  -- | 'userId'
  Types.IdType ->
  -- | 'arn'
  Types.Arn ->
  -- | 'createDate'
  Core.UTCTime ->
  User
mkUser path userName userId arn createDate =
  User'
    { path,
      userName,
      userId,
      arn,
      createDate,
      passwordLastUsed = Core.Nothing,
      permissionsBoundary = Core.Nothing,
      tags = Core.Nothing
    }

-- | The path to the user. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uPath :: Lens.Lens' User Types.Path
uPath = Lens.field @"path"
{-# DEPRECATED uPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The friendly name identifying the user.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uUserName :: Lens.Lens' User Types.UserName
uUserName = Lens.field @"userName"
{-# DEPRECATED uUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The stable and unique string identifying the user. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uUserId :: Lens.Lens' User Types.IdType
uUserId = Lens.field @"userId"
{-# DEPRECATED uUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The Amazon Resource Name (ARN) that identifies the user. For more information about ARNs and how to use ARNs in policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uArn :: Lens.Lens' User Types.Arn
uArn = Lens.field @"arn"
{-# DEPRECATED uArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the user was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uCreateDate :: Lens.Lens' User Core.UTCTime
uCreateDate = Lens.field @"createDate"
{-# DEPRECATED uCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

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
uPasswordLastUsed :: Lens.Lens' User (Core.Maybe Core.UTCTime)
uPasswordLastUsed = Lens.field @"passwordLastUsed"
{-# DEPRECATED uPasswordLastUsed "Use generic-lens or generic-optics with 'passwordLastUsed' instead." #-}

-- | The ARN of the policy used to set the permissions boundary for the user.
--
-- For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Identities > in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'permissionsBoundary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uPermissionsBoundary :: Lens.Lens' User (Core.Maybe Types.AttachedPermissionsBoundary)
uPermissionsBoundary = Lens.field @"permissionsBoundary"
{-# DEPRECATED uPermissionsBoundary "Use generic-lens or generic-optics with 'permissionsBoundary' instead." #-}

-- | A list of tags that are associated with the specified user. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uTags :: Lens.Lens' User (Core.Maybe [Types.Tag])
uTags = Lens.field @"tags"
{-# DEPRECATED uTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromXML User where
  parseXML x =
    User'
      Core.<$> (x Core..@ "Path")
      Core.<*> (x Core..@ "UserName")
      Core.<*> (x Core..@ "UserId")
      Core.<*> (x Core..@ "Arn")
      Core.<*> (x Core..@ "CreateDate")
      Core.<*> (x Core..@? "PasswordLastUsed")
      Core.<*> (x Core..@? "PermissionsBoundary")
      Core.<*> (x Core..@? "Tags" Core..<@> Core.parseXMLList "member")
