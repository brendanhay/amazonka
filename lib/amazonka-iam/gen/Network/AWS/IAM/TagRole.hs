{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkTagRole' smart constructor.
data TagRole = TagRole'
  { -- | The name of the role that you want to add tags to.
    --
    -- This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    roleName :: Types.RoleName,
    -- | The list of tags that you want to attach to the role. Each tag consists of a key name and an associated value. You can specify this with a JSON string.
    tags :: [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagRole' value with any optional fields omitted.
mkTagRole ::
  -- | 'roleName'
  Types.RoleName ->
  TagRole
mkTagRole roleName = TagRole' {roleName, tags = Core.mempty}

-- | The name of the role that you want to add tags to.
--
-- This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trRoleName :: Lens.Lens' TagRole Types.RoleName
trRoleName = Lens.field @"roleName"
{-# DEPRECATED trRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | The list of tags that you want to attach to the role. Each tag consists of a key name and an associated value. You can specify this with a JSON string.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trTags :: Lens.Lens' TagRole [Types.Tag]
trTags = Lens.field @"tags"
{-# DEPRECATED trTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.AWSRequest TagRole where
  type Rs TagRole = TagRoleResponse
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
            ( Core.pure ("Action", "TagRole")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "RoleName" roleName)
                Core.<> (Core.toQueryValue "Tags" (Core.toQueryList "member" tags))
            )
      }
  response = Response.receiveNull TagRoleResponse'

-- | /See:/ 'mkTagRoleResponse' smart constructor.
data TagRoleResponse = TagRoleResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagRoleResponse' value with any optional fields omitted.
mkTagRoleResponse ::
  TagRoleResponse
mkTagRoleResponse = TagRoleResponse'
