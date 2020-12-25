{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetLoginProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the user name and password-creation date for the specified IAM user. If the user has not been assigned a password, the operation returns a 404 (@NoSuchEntity@ ) error.
module Network.AWS.IAM.GetLoginProfile
  ( -- * Creating a request
    GetLoginProfile (..),
    mkGetLoginProfile,

    -- ** Request lenses
    glpUserName,

    -- * Destructuring the response
    GetLoginProfileResponse (..),
    mkGetLoginProfileResponse,

    -- ** Response lenses
    glprrsLoginProfile,
    glprrsResponseStatus,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetLoginProfile' smart constructor.
newtype GetLoginProfile = GetLoginProfile'
  { -- | The name of the user whose login profile you want to retrieve.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Types.UserName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetLoginProfile' value with any optional fields omitted.
mkGetLoginProfile ::
  -- | 'userName'
  Types.UserName ->
  GetLoginProfile
mkGetLoginProfile userName = GetLoginProfile' {userName}

-- | The name of the user whose login profile you want to retrieve.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glpUserName :: Lens.Lens' GetLoginProfile Types.UserName
glpUserName = Lens.field @"userName"
{-# DEPRECATED glpUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Core.AWSRequest GetLoginProfile where
  type Rs GetLoginProfile = GetLoginProfileResponse
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
            ( Core.pure ("Action", "GetLoginProfile")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "UserName" userName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "GetLoginProfileResult"
      ( \s h x ->
          GetLoginProfileResponse'
            Core.<$> (x Core..@ "LoginProfile") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a successful 'GetLoginProfile' request.
--
-- /See:/ 'mkGetLoginProfileResponse' smart constructor.
data GetLoginProfileResponse = GetLoginProfileResponse'
  { -- | A structure containing the user name and password create date for the user.
    loginProfile :: Types.LoginProfile,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetLoginProfileResponse' value with any optional fields omitted.
mkGetLoginProfileResponse ::
  -- | 'loginProfile'
  Types.LoginProfile ->
  -- | 'responseStatus'
  Core.Int ->
  GetLoginProfileResponse
mkGetLoginProfileResponse loginProfile responseStatus =
  GetLoginProfileResponse' {loginProfile, responseStatus}

-- | A structure containing the user name and password create date for the user.
--
-- /Note:/ Consider using 'loginProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glprrsLoginProfile :: Lens.Lens' GetLoginProfileResponse Types.LoginProfile
glprrsLoginProfile = Lens.field @"loginProfile"
{-# DEPRECATED glprrsLoginProfile "Use generic-lens or generic-optics with 'loginProfile' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glprrsResponseStatus :: Lens.Lens' GetLoginProfileResponse Core.Int
glprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED glprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
