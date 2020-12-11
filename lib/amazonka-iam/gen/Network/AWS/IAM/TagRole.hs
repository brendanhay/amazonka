{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.TagRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to an IAM role. The role can be a regular role or a service-linked role. If a tag with the same key name already exists, then that tag is overwritten with the new value.
--
-- A tag consists of a key name and an associated value. By assigning tags to your resources, you can do the following:
--
--     * __Administrative grouping and discovery__ - Attach tags to resources to aid in organization and search. For example, you could search for all resources with the key name /Project/ and the value /MyImportantProject/ . Or search for all resources with the key name /Cost Center/ and the value /41200/ .
--
--
--     * __Access control__ - Reference tags in IAM user-based and resource-based policies. You can use tags to restrict access to only an IAM user or role that has a specified tag attached. You can also restrict access to only those resources that have a certain tag attached. For examples of policies that show how to use tags to control access, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_tags.html Control Access Using IAM Tags> in the /IAM User Guide/ .
--
--
--     * __Cost allocation__ - Use tags to help track which individuals and teams are using which AWS resources.
--
--
-- For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
module Network.AWS.IAM.TagRole
  ( -- * Creating a request
    TagRole (..),
    mkTagRole,

    -- ** Request lenses
    trRoleName,
    trTags,

    -- * Destructuring the response
    TagRoleResponse (..),
    mkTagRoleResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkTagRole' smart constructor.
data TagRole = TagRole' {roleName :: Lude.Text, tags :: [Tag]}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagRole' with the minimum fields required to make a request.
--
-- * 'roleName' - The name of the role that you want to add tags to.
--
-- This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'tags' - The list of tags that you want to attach to the role. Each tag consists of a key name and an associated value. You can specify this with a JSON string.
mkTagRole ::
  -- | 'roleName'
  Lude.Text ->
  TagRole
mkTagRole pRoleName_ =
  TagRole' {roleName = pRoleName_, tags = Lude.mempty}

-- | The name of the role that you want to add tags to.
--
-- This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trRoleName :: Lens.Lens' TagRole Lude.Text
trRoleName = Lens.lens (roleName :: TagRole -> Lude.Text) (\s a -> s {roleName = a} :: TagRole)
{-# DEPRECATED trRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | The list of tags that you want to attach to the role. Each tag consists of a key name and an associated value. You can specify this with a JSON string.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trTags :: Lens.Lens' TagRole [Tag]
trTags = Lens.lens (tags :: TagRole -> [Tag]) (\s a -> s {tags = a} :: TagRole)
{-# DEPRECATED trTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest TagRole where
  type Rs TagRole = TagRoleResponse
  request = Req.postQuery iamService
  response = Res.receiveNull TagRoleResponse'

instance Lude.ToHeaders TagRole where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath TagRole where
  toPath = Lude.const "/"

instance Lude.ToQuery TagRole where
  toQuery TagRole' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("TagRole" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "RoleName" Lude.=: roleName,
        "Tags" Lude.=: Lude.toQueryList "member" tags
      ]

-- | /See:/ 'mkTagRoleResponse' smart constructor.
data TagRoleResponse = TagRoleResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagRoleResponse' with the minimum fields required to make a request.
mkTagRoleResponse ::
  TagRoleResponse
mkTagRoleResponse = TagRoleResponse'
