{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetLoginProfile (..)
    , mkGetLoginProfile
    -- ** Request lenses
    , glpUserName

    -- * Destructuring the response
    , GetLoginProfileResponse (..)
    , mkGetLoginProfileResponse
    -- ** Response lenses
    , glprrsLoginProfile
    , glprrsResponseStatus
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetLoginProfile' smart constructor.
newtype GetLoginProfile = GetLoginProfile'
  { userName :: Types.UserName
    -- ^ The name of the user whose login profile you want to retrieve.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetLoginProfile' value with any optional fields omitted.
mkGetLoginProfile
    :: Types.UserName -- ^ 'userName'
    -> GetLoginProfile
mkGetLoginProfile userName = GetLoginProfile'{userName}

-- | The name of the user whose login profile you want to retrieve.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glpUserName :: Lens.Lens' GetLoginProfile Types.UserName
glpUserName = Lens.field @"userName"
{-# INLINEABLE glpUserName #-}
{-# DEPRECATED userName "Use generic-lens or generic-optics with 'userName' instead"  #-}

instance Core.ToQuery GetLoginProfile where
        toQuery GetLoginProfile{..}
          = Core.toQueryPair "Action" ("GetLoginProfile" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "UserName" userName

instance Core.ToHeaders GetLoginProfile where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetLoginProfile where
        type Rs GetLoginProfile = GetLoginProfileResponse
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
          = Response.receiveXMLWrapper "GetLoginProfileResult"
              (\ s h x ->
                 GetLoginProfileResponse' Core.<$>
                   (x Core..@ "LoginProfile") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a successful 'GetLoginProfile' request. 
--
-- /See:/ 'mkGetLoginProfileResponse' smart constructor.
data GetLoginProfileResponse = GetLoginProfileResponse'
  { loginProfile :: Types.LoginProfile
    -- ^ A structure containing the user name and password create date for the user.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetLoginProfileResponse' value with any optional fields omitted.
mkGetLoginProfileResponse
    :: Types.LoginProfile -- ^ 'loginProfile'
    -> Core.Int -- ^ 'responseStatus'
    -> GetLoginProfileResponse
mkGetLoginProfileResponse loginProfile responseStatus
  = GetLoginProfileResponse'{loginProfile, responseStatus}

-- | A structure containing the user name and password create date for the user.
--
-- /Note:/ Consider using 'loginProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glprrsLoginProfile :: Lens.Lens' GetLoginProfileResponse Types.LoginProfile
glprrsLoginProfile = Lens.field @"loginProfile"
{-# INLINEABLE glprrsLoginProfile #-}
{-# DEPRECATED loginProfile "Use generic-lens or generic-optics with 'loginProfile' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glprrsResponseStatus :: Lens.Lens' GetLoginProfileResponse Core.Int
glprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE glprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
