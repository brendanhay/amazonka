{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified IAM user, including the user's creation date, path, unique ID, and ARN.
--
-- If you do not specify a user name, IAM determines the user name implicitly based on the AWS access key ID used to sign the request to this API.
module Network.AWS.IAM.GetUser
    (
    -- * Creating a request
      GetUser (..)
    , mkGetUser
    -- ** Request lenses
    , guUserName

    -- * Destructuring the response
    , GetUserResponse (..)
    , mkGetUserResponse
    -- ** Response lenses
    , gurrsUser
    , gurrsResponseStatus
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetUser' smart constructor.
newtype GetUser = GetUser'
  { userName :: Core.Maybe Types.UserName
    -- ^ The name of the user to get information about.
--
-- This parameter is optional. If it is not included, it defaults to the user making the request. This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetUser' value with any optional fields omitted.
mkGetUser
    :: GetUser
mkGetUser = GetUser'{userName = Core.Nothing}

-- | The name of the user to get information about.
--
-- This parameter is optional. If it is not included, it defaults to the user making the request. This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guUserName :: Lens.Lens' GetUser (Core.Maybe Types.UserName)
guUserName = Lens.field @"userName"
{-# INLINEABLE guUserName #-}
{-# DEPRECATED userName "Use generic-lens or generic-optics with 'userName' instead"  #-}

instance Core.ToQuery GetUser where
        toQuery GetUser{..}
          = Core.toQueryPair "Action" ("GetUser" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "UserName") userName

instance Core.ToHeaders GetUser where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetUser where
        type Rs GetUser = GetUserResponse
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
        parseResponse
          = Response.receiveXMLWrapper "GetUserResult"
              (\ s h x ->
                 GetUserResponse' Core.<$>
                   (x Core..@ "User") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a successful 'GetUser' request. 
--
-- /See:/ 'mkGetUserResponse' smart constructor.
data GetUserResponse = GetUserResponse'
  { user :: Types.User
    -- ^ A structure containing details about the IAM user.
--
-- /Important:/ Due to a service issue, password last used data does not include password use from May 3, 2018 22:50 PDT to May 23, 2018 14:08 PDT. This affects <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_finding-unused.html last sign-in> dates shown in the IAM console and password last used dates in the <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_getting-report.html IAM credential report> , and returned by this GetUser API. If users signed in during the affected time, the password last used date that is returned is the date the user last signed in before May 3, 2018. For users that signed in after May 23, 2018 14:08 PDT, the returned password last used date is accurate.
-- You can use password last used information to identify unused credentials for deletion. For example, you might delete users who did not sign in to AWS in the last 90 days. In cases like this, we recommend that you adjust your evaluation window to include dates after May 23, 2018. Alternatively, if your users use access keys to access AWS programmatically you can refer to access key last used information because it is accurate for all dates. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetUserResponse' value with any optional fields omitted.
mkGetUserResponse
    :: Types.User -- ^ 'user'
    -> Core.Int -- ^ 'responseStatus'
    -> GetUserResponse
mkGetUserResponse user responseStatus
  = GetUserResponse'{user, responseStatus}

-- | A structure containing details about the IAM user.
--
-- /Important:/ Due to a service issue, password last used data does not include password use from May 3, 2018 22:50 PDT to May 23, 2018 14:08 PDT. This affects <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_finding-unused.html last sign-in> dates shown in the IAM console and password last used dates in the <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_getting-report.html IAM credential report> , and returned by this GetUser API. If users signed in during the affected time, the password last used date that is returned is the date the user last signed in before May 3, 2018. For users that signed in after May 23, 2018 14:08 PDT, the returned password last used date is accurate.
-- You can use password last used information to identify unused credentials for deletion. For example, you might delete users who did not sign in to AWS in the last 90 days. In cases like this, we recommend that you adjust your evaluation window to include dates after May 23, 2018. Alternatively, if your users use access keys to access AWS programmatically you can refer to access key last used information because it is accurate for all dates. 
--
-- /Note:/ Consider using 'user' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gurrsUser :: Lens.Lens' GetUserResponse Types.User
gurrsUser = Lens.field @"user"
{-# INLINEABLE gurrsUser #-}
{-# DEPRECATED user "Use generic-lens or generic-optics with 'user' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gurrsResponseStatus :: Lens.Lens' GetUserResponse Core.Int
gurrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gurrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
