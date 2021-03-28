{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.UpdateUserPhoneConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the phone configuration settings for the specified user.
module Network.AWS.Connect.UpdateUserPhoneConfig
    (
    -- * Creating a request
      UpdateUserPhoneConfig (..)
    , mkUpdateUserPhoneConfig
    -- ** Request lenses
    , uupcPhoneConfig
    , uupcUserId
    , uupcInstanceId

    -- * Destructuring the response
    , UpdateUserPhoneConfigResponse (..)
    , mkUpdateUserPhoneConfigResponse
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateUserPhoneConfig' smart constructor.
data UpdateUserPhoneConfig = UpdateUserPhoneConfig'
  { phoneConfig :: Types.UserPhoneConfig
    -- ^ Information about phone configuration settings for the user.
  , userId :: Types.UserId
    -- ^ The identifier of the user account.
  , instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateUserPhoneConfig' value with any optional fields omitted.
mkUpdateUserPhoneConfig
    :: Types.UserPhoneConfig -- ^ 'phoneConfig'
    -> Types.UserId -- ^ 'userId'
    -> Types.InstanceId -- ^ 'instanceId'
    -> UpdateUserPhoneConfig
mkUpdateUserPhoneConfig phoneConfig userId instanceId
  = UpdateUserPhoneConfig'{phoneConfig, userId, instanceId}

-- | Information about phone configuration settings for the user.
--
-- /Note:/ Consider using 'phoneConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcPhoneConfig :: Lens.Lens' UpdateUserPhoneConfig Types.UserPhoneConfig
uupcPhoneConfig = Lens.field @"phoneConfig"
{-# INLINEABLE uupcPhoneConfig #-}
{-# DEPRECATED phoneConfig "Use generic-lens or generic-optics with 'phoneConfig' instead"  #-}

-- | The identifier of the user account.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcUserId :: Lens.Lens' UpdateUserPhoneConfig Types.UserId
uupcUserId = Lens.field @"userId"
{-# INLINEABLE uupcUserId #-}
{-# DEPRECATED userId "Use generic-lens or generic-optics with 'userId' instead"  #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcInstanceId :: Lens.Lens' UpdateUserPhoneConfig Types.InstanceId
uupcInstanceId = Lens.field @"instanceId"
{-# INLINEABLE uupcInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

instance Core.ToQuery UpdateUserPhoneConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateUserPhoneConfig where
        toHeaders UpdateUserPhoneConfig{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateUserPhoneConfig where
        toJSON UpdateUserPhoneConfig{..}
          = Core.object
              (Core.catMaybes [Core.Just ("PhoneConfig" Core..= phoneConfig)])

instance Core.AWSRequest UpdateUserPhoneConfig where
        type Rs UpdateUserPhoneConfig = UpdateUserPhoneConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/users/" Core.<> Core.toText instanceId Core.<> "/" Core.<>
                             Core.toText userId
                             Core.<> "/phone-config",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull UpdateUserPhoneConfigResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateUserPhoneConfigResponse' smart constructor.
data UpdateUserPhoneConfigResponse = UpdateUserPhoneConfigResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateUserPhoneConfigResponse' value with any optional fields omitted.
mkUpdateUserPhoneConfigResponse
    :: UpdateUserPhoneConfigResponse
mkUpdateUserPhoneConfigResponse = UpdateUserPhoneConfigResponse'
