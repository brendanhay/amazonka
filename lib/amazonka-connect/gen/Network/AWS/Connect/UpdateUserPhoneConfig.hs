{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    UpdateUserPhoneConfig (..),
    mkUpdateUserPhoneConfig,

    -- ** Request lenses
    uupcPhoneConfig,
    uupcUserId,
    uupcInstanceId,

    -- * Destructuring the response
    UpdateUserPhoneConfigResponse (..),
    mkUpdateUserPhoneConfigResponse,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateUserPhoneConfig' smart constructor.
data UpdateUserPhoneConfig = UpdateUserPhoneConfig'
  { -- | Information about phone configuration settings for the user.
    phoneConfig :: Types.UserPhoneConfig,
    -- | The identifier of the user account.
    userId :: Types.UserId,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateUserPhoneConfig' value with any optional fields omitted.
mkUpdateUserPhoneConfig ::
  -- | 'phoneConfig'
  Types.UserPhoneConfig ->
  -- | 'userId'
  Types.UserId ->
  -- | 'instanceId'
  Types.InstanceId ->
  UpdateUserPhoneConfig
mkUpdateUserPhoneConfig phoneConfig userId instanceId =
  UpdateUserPhoneConfig' {phoneConfig, userId, instanceId}

-- | Information about phone configuration settings for the user.
--
-- /Note:/ Consider using 'phoneConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcPhoneConfig :: Lens.Lens' UpdateUserPhoneConfig Types.UserPhoneConfig
uupcPhoneConfig = Lens.field @"phoneConfig"
{-# DEPRECATED uupcPhoneConfig "Use generic-lens or generic-optics with 'phoneConfig' instead." #-}

-- | The identifier of the user account.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcUserId :: Lens.Lens' UpdateUserPhoneConfig Types.UserId
uupcUserId = Lens.field @"userId"
{-# DEPRECATED uupcUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcInstanceId :: Lens.Lens' UpdateUserPhoneConfig Types.InstanceId
uupcInstanceId = Lens.field @"instanceId"
{-# DEPRECATED uupcInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Core.FromJSON UpdateUserPhoneConfig where
  toJSON UpdateUserPhoneConfig {..} =
    Core.object
      (Core.catMaybes [Core.Just ("PhoneConfig" Core..= phoneConfig)])

instance Core.AWSRequest UpdateUserPhoneConfig where
  type Rs UpdateUserPhoneConfig = UpdateUserPhoneConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/users/" Core.<> (Core.toText instanceId) Core.<> ("/")
                Core.<> (Core.toText userId)
                Core.<> ("/phone-config")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull UpdateUserPhoneConfigResponse'

-- | /See:/ 'mkUpdateUserPhoneConfigResponse' smart constructor.
data UpdateUserPhoneConfigResponse = UpdateUserPhoneConfigResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateUserPhoneConfigResponse' value with any optional fields omitted.
mkUpdateUserPhoneConfigResponse ::
  UpdateUserPhoneConfigResponse
mkUpdateUserPhoneConfigResponse = UpdateUserPhoneConfigResponse'
