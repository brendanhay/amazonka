{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.TagUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to an IAM user. If a tag with the same key name already exists, then that tag is overwritten with the new value.
--
-- A tag consists of a key name and an associated value. By assigning tags to your resources, you can do the following:
--
--     * __Administrative grouping and discovery__ - Attach tags to resources to aid in organization and search. For example, you could search for all resources with the key name /Project/ and the value /MyImportantProject/ . Or search for all resources with the key name /Cost Center/ and the value /41200/ . 
--
--
--     * __Access control__ - Reference tags in IAM user-based and resource-based policies. You can use tags to restrict access to only an IAM requesting user or to a role that has a specified tag attached. You can also restrict access to only those resources that have a certain tag attached. For examples of policies that show how to use tags to control access, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_tags.html Control Access Using IAM Tags> in the /IAM User Guide/ .
--
--
--     * __Cost allocation__ - Use tags to help track which individuals and teams are using which AWS resources.
--
--
-- For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
module Network.AWS.IAM.TagUser
    (
    -- * Creating a request
      TagUser (..)
    , mkTagUser
    -- ** Request lenses
    , tuUserName
    , tuTags

    -- * Destructuring the response
    , TagUserResponse (..)
    , mkTagUserResponse
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkTagUser' smart constructor.
data TagUser = TagUser'
  { userName :: Types.UserName
    -- ^ The name of the user that you want to add tags to.
--
-- This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: =,.@-
  , tags :: [Types.Tag]
    -- ^ The list of tags that you want to attach to the user. Each tag consists of a key name and an associated value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagUser' value with any optional fields omitted.
mkTagUser
    :: Types.UserName -- ^ 'userName'
    -> TagUser
mkTagUser userName = TagUser'{userName, tags = Core.mempty}

-- | The name of the user that you want to add tags to.
--
-- This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: =,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tuUserName :: Lens.Lens' TagUser Types.UserName
tuUserName = Lens.field @"userName"
{-# INLINEABLE tuUserName #-}
{-# DEPRECATED userName "Use generic-lens or generic-optics with 'userName' instead"  #-}

-- | The list of tags that you want to attach to the user. Each tag consists of a key name and an associated value.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tuTags :: Lens.Lens' TagUser [Types.Tag]
tuTags = Lens.field @"tags"
{-# INLINEABLE tuTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery TagUser where
        toQuery TagUser{..}
          = Core.toQueryPair "Action" ("TagUser" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "UserName" userName
              Core.<> Core.toQueryPair "Tags" (Core.toQueryList "member" tags)

instance Core.ToHeaders TagUser where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest TagUser where
        type Rs TagUser = TagUserResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull TagUserResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkTagUserResponse' smart constructor.
data TagUserResponse = TagUserResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagUserResponse' value with any optional fields omitted.
mkTagUserResponse
    :: TagUserResponse
mkTagUserResponse = TagUserResponse'
