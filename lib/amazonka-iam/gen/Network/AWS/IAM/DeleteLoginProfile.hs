{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteLoginProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the password for the specified IAM user, which terminates the user's ability to access AWS services through the AWS Management Console.
--
-- /Important:/ Deleting a user's password does not prevent a user from accessing AWS through the command line interface or the API. To prevent all user access, you must also either make any access keys inactive or delete them. For more information about making keys inactive or deleting them, see 'UpdateAccessKey' and 'DeleteAccessKey' . 
module Network.AWS.IAM.DeleteLoginProfile
    (
    -- * Creating a request
      DeleteLoginProfile (..)
    , mkDeleteLoginProfile
    -- ** Request lenses
    , dlpUserName

    -- * Destructuring the response
    , DeleteLoginProfileResponse (..)
    , mkDeleteLoginProfileResponse
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteLoginProfile' smart constructor.
newtype DeleteLoginProfile = DeleteLoginProfile'
  { userName :: Types.UserName
    -- ^ The name of the user whose password you want to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLoginProfile' value with any optional fields omitted.
mkDeleteLoginProfile
    :: Types.UserName -- ^ 'userName'
    -> DeleteLoginProfile
mkDeleteLoginProfile userName = DeleteLoginProfile'{userName}

-- | The name of the user whose password you want to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlpUserName :: Lens.Lens' DeleteLoginProfile Types.UserName
dlpUserName = Lens.field @"userName"
{-# INLINEABLE dlpUserName #-}
{-# DEPRECATED userName "Use generic-lens or generic-optics with 'userName' instead"  #-}

instance Core.ToQuery DeleteLoginProfile where
        toQuery DeleteLoginProfile{..}
          = Core.toQueryPair "Action" ("DeleteLoginProfile" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "UserName" userName

instance Core.ToHeaders DeleteLoginProfile where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteLoginProfile where
        type Rs DeleteLoginProfile = DeleteLoginProfileResponse
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
        parseResponse = Response.receiveNull DeleteLoginProfileResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteLoginProfileResponse' smart constructor.
data DeleteLoginProfileResponse = DeleteLoginProfileResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLoginProfileResponse' value with any optional fields omitted.
mkDeleteLoginProfileResponse
    :: DeleteLoginProfileResponse
mkDeleteLoginProfileResponse = DeleteLoginProfileResponse'
