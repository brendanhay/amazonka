{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UntagUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified tags from the user. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
module Network.AWS.IAM.UntagUser
    (
    -- * Creating a request
      UntagUser (..)
    , mkUntagUser
    -- ** Request lenses
    , uuUserName
    , uuTagKeys

    -- * Destructuring the response
    , UntagUserResponse (..)
    , mkUntagUserResponse
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUntagUser' smart constructor.
data UntagUser = UntagUser'
  { userName :: Types.ExistingUserNameType
    -- ^ The name of the IAM user from which you want to remove tags.
--
-- This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: =,.@-
  , tagKeys :: [Types.TagKeyType]
    -- ^ A list of key names as a simple array of strings. The tags with matching keys are removed from the specified user.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UntagUser' value with any optional fields omitted.
mkUntagUser
    :: Types.ExistingUserNameType -- ^ 'userName'
    -> UntagUser
mkUntagUser userName = UntagUser'{userName, tagKeys = Core.mempty}

-- | The name of the IAM user from which you want to remove tags.
--
-- This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: =,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuUserName :: Lens.Lens' UntagUser Types.ExistingUserNameType
uuUserName = Lens.field @"userName"
{-# INLINEABLE uuUserName #-}
{-# DEPRECATED userName "Use generic-lens or generic-optics with 'userName' instead"  #-}

-- | A list of key names as a simple array of strings. The tags with matching keys are removed from the specified user.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuTagKeys :: Lens.Lens' UntagUser [Types.TagKeyType]
uuTagKeys = Lens.field @"tagKeys"
{-# INLINEABLE uuTagKeys #-}
{-# DEPRECATED tagKeys "Use generic-lens or generic-optics with 'tagKeys' instead"  #-}

instance Core.ToQuery UntagUser where
        toQuery UntagUser{..}
          = Core.toQueryPair "Action" ("UntagUser" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "UserName" userName
              Core.<>
              Core.toQueryPair "TagKeys" (Core.toQueryList "member" tagKeys)

instance Core.ToHeaders UntagUser where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest UntagUser where
        type Rs UntagUser = UntagUserResponse
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
        parseResponse = Response.receiveNull UntagUserResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUntagUserResponse' smart constructor.
data UntagUserResponse = UntagUserResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UntagUserResponse' value with any optional fields omitted.
mkUntagUserResponse
    :: UntagUserResponse
mkUntagUserResponse = UntagUserResponse'
