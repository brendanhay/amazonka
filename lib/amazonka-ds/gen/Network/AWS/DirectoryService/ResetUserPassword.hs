{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.ResetUserPassword
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets the password for any user in your AWS Managed Microsoft AD or Simple AD directory.
--
-- You can reset the password for any user in your directory with the following exceptions:
--
--     * For Simple AD, you cannot reset the password for any user that is a member of either the __Domain Admins__ or __Enterprise Admins__ group except for the administrator user.
--
--
--     * For AWS Managed Microsoft AD, you can only reset the password for a user that is in an OU based off of the NetBIOS name that you typed when you created your directory. For example, you cannot reset the password for a user in the __AWS Reserved__ OU. For more information about the OU structure for an AWS Managed Microsoft AD directory, see <https://docs.aws.amazon.com/directoryservice/latest/admin-guide/ms_ad_getting_started_what_gets_created.html What Gets Created> in the /AWS Directory Service Administration Guide/ .
--
--
module Network.AWS.DirectoryService.ResetUserPassword
    (
    -- * Creating a request
      ResetUserPassword (..)
    , mkResetUserPassword
    -- ** Request lenses
    , rupDirectoryId
    , rupUserName
    , rupNewPassword

    -- * Destructuring the response
    , ResetUserPasswordResponse (..)
    , mkResetUserPasswordResponse
    -- ** Response lenses
    , ruprrsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkResetUserPassword' smart constructor.
data ResetUserPassword = ResetUserPassword'
  { directoryId :: Types.DirectoryId
    -- ^ Identifier of the AWS Managed Microsoft AD or Simple AD directory in which the user resides.
  , userName :: Types.CustomerUserName
    -- ^ The user name of the user whose password will be reset.
  , newPassword :: Types.NewPassword
    -- ^ The new password that will be reset.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResetUserPassword' value with any optional fields omitted.
mkResetUserPassword
    :: Types.DirectoryId -- ^ 'directoryId'
    -> Types.CustomerUserName -- ^ 'userName'
    -> Types.NewPassword -- ^ 'newPassword'
    -> ResetUserPassword
mkResetUserPassword directoryId userName newPassword
  = ResetUserPassword'{directoryId, userName, newPassword}

-- | Identifier of the AWS Managed Microsoft AD or Simple AD directory in which the user resides.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rupDirectoryId :: Lens.Lens' ResetUserPassword Types.DirectoryId
rupDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE rupDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The user name of the user whose password will be reset.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rupUserName :: Lens.Lens' ResetUserPassword Types.CustomerUserName
rupUserName = Lens.field @"userName"
{-# INLINEABLE rupUserName #-}
{-# DEPRECATED userName "Use generic-lens or generic-optics with 'userName' instead"  #-}

-- | The new password that will be reset.
--
-- /Note:/ Consider using 'newPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rupNewPassword :: Lens.Lens' ResetUserPassword Types.NewPassword
rupNewPassword = Lens.field @"newPassword"
{-# INLINEABLE rupNewPassword #-}
{-# DEPRECATED newPassword "Use generic-lens or generic-optics with 'newPassword' instead"  #-}

instance Core.ToQuery ResetUserPassword where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ResetUserPassword where
        toHeaders ResetUserPassword{..}
          = Core.pure
              ("X-Amz-Target", "DirectoryService_20150416.ResetUserPassword")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ResetUserPassword where
        toJSON ResetUserPassword{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DirectoryId" Core..= directoryId),
                  Core.Just ("UserName" Core..= userName),
                  Core.Just ("NewPassword" Core..= newPassword)])

instance Core.AWSRequest ResetUserPassword where
        type Rs ResetUserPassword = ResetUserPasswordResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 ResetUserPasswordResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkResetUserPasswordResponse' smart constructor.
newtype ResetUserPasswordResponse = ResetUserPasswordResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ResetUserPasswordResponse' value with any optional fields omitted.
mkResetUserPasswordResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ResetUserPasswordResponse
mkResetUserPasswordResponse responseStatus
  = ResetUserPasswordResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ruprrsResponseStatus :: Lens.Lens' ResetUserPasswordResponse Core.Int
ruprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ruprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
