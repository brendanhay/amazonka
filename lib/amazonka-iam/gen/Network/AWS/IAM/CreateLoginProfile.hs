{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.CreateLoginProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a password for the specified user, giving the user the ability to access AWS services through the AWS Management Console. For more information about managing passwords, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_ManagingLogins.html Managing Passwords> in the /IAM User Guide/ .
module Network.AWS.IAM.CreateLoginProfile
    (
    -- * Creating a request
      CreateLoginProfile (..)
    , mkCreateLoginProfile
    -- ** Request lenses
    , clpUserName
    , clpPassword
    , clpPasswordResetRequired

    -- * Destructuring the response
    , CreateLoginProfileResponse (..)
    , mkCreateLoginProfileResponse
    -- ** Response lenses
    , clprrsLoginProfile
    , clprrsResponseStatus
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateLoginProfile' smart constructor.
data CreateLoginProfile = CreateLoginProfile'
  { userName :: Types.UserName
    -- ^ The name of the IAM user to create a password for. The user must already exist.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  , password :: Types.PasswordType
    -- ^ The new password for the user.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of characters. That string can include almost any printable ASCII character from the space (@\u0020@ ) through the end of the ASCII character range (@\u00FF@ ). You can also include the tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ ) characters. Any of these characters are valid in a password. However, many tools, such as the AWS Management Console, might restrict the ability to type certain characters because they have special meaning within that tool.
  , passwordResetRequired :: Core.Maybe Core.Bool
    -- ^ Specifies whether the user is required to set a new password on next sign-in.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLoginProfile' value with any optional fields omitted.
mkCreateLoginProfile
    :: Types.UserName -- ^ 'userName'
    -> Types.PasswordType -- ^ 'password'
    -> CreateLoginProfile
mkCreateLoginProfile userName password
  = CreateLoginProfile'{userName, password,
                        passwordResetRequired = Core.Nothing}

-- | The name of the IAM user to create a password for. The user must already exist.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clpUserName :: Lens.Lens' CreateLoginProfile Types.UserName
clpUserName = Lens.field @"userName"
{-# INLINEABLE clpUserName #-}
{-# DEPRECATED userName "Use generic-lens or generic-optics with 'userName' instead"  #-}

-- | The new password for the user.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of characters. That string can include almost any printable ASCII character from the space (@\u0020@ ) through the end of the ASCII character range (@\u00FF@ ). You can also include the tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ ) characters. Any of these characters are valid in a password. However, many tools, such as the AWS Management Console, might restrict the ability to type certain characters because they have special meaning within that tool.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clpPassword :: Lens.Lens' CreateLoginProfile Types.PasswordType
clpPassword = Lens.field @"password"
{-# INLINEABLE clpPassword #-}
{-# DEPRECATED password "Use generic-lens or generic-optics with 'password' instead"  #-}

-- | Specifies whether the user is required to set a new password on next sign-in.
--
-- /Note:/ Consider using 'passwordResetRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clpPasswordResetRequired :: Lens.Lens' CreateLoginProfile (Core.Maybe Core.Bool)
clpPasswordResetRequired = Lens.field @"passwordResetRequired"
{-# INLINEABLE clpPasswordResetRequired #-}
{-# DEPRECATED passwordResetRequired "Use generic-lens or generic-optics with 'passwordResetRequired' instead"  #-}

instance Core.ToQuery CreateLoginProfile where
        toQuery CreateLoginProfile{..}
          = Core.toQueryPair "Action" ("CreateLoginProfile" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "UserName" userName
              Core.<> Core.toQueryPair "Password" password
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PasswordResetRequired")
                passwordResetRequired

instance Core.ToHeaders CreateLoginProfile where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateLoginProfile where
        type Rs CreateLoginProfile = CreateLoginProfileResponse
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
          = Response.receiveXMLWrapper "CreateLoginProfileResult"
              (\ s h x ->
                 CreateLoginProfileResponse' Core.<$>
                   (x Core..@ "LoginProfile") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a successful 'CreateLoginProfile' request. 
--
-- /See:/ 'mkCreateLoginProfileResponse' smart constructor.
data CreateLoginProfileResponse = CreateLoginProfileResponse'
  { loginProfile :: Types.LoginProfile
    -- ^ A structure containing the user name and password create date.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateLoginProfileResponse' value with any optional fields omitted.
mkCreateLoginProfileResponse
    :: Types.LoginProfile -- ^ 'loginProfile'
    -> Core.Int -- ^ 'responseStatus'
    -> CreateLoginProfileResponse
mkCreateLoginProfileResponse loginProfile responseStatus
  = CreateLoginProfileResponse'{loginProfile, responseStatus}

-- | A structure containing the user name and password create date.
--
-- /Note:/ Consider using 'loginProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clprrsLoginProfile :: Lens.Lens' CreateLoginProfileResponse Types.LoginProfile
clprrsLoginProfile = Lens.field @"loginProfile"
{-# INLINEABLE clprrsLoginProfile #-}
{-# DEPRECATED loginProfile "Use generic-lens or generic-optics with 'loginProfile' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clprrsResponseStatus :: Lens.Lens' CreateLoginProfileResponse Core.Int
clprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE clprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
