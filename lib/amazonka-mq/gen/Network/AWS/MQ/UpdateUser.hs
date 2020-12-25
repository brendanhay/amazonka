{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.UpdateUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the information for an ActiveMQ user.
module Network.AWS.MQ.UpdateUser
  ( -- * Creating a request
    UpdateUser (..),
    mkUpdateUser,

    -- ** Request lenses
    uuUsername,
    uuBrokerId,
    uuConsoleAccess,
    uuGroups,
    uuPassword,

    -- * Destructuring the response
    UpdateUserResponse (..),
    mkUpdateUserResponse,

    -- ** Response lenses
    uurrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MQ.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Updates the information for an ActiveMQ user.
--
-- /See:/ 'mkUpdateUser' smart constructor.
data UpdateUser = UpdateUser'
  { -- | Required. The username of the ActiveMQ user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
    username :: Core.Text,
    -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Core.Text,
    -- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
    consoleAccess :: Core.Maybe Core.Bool,
    -- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
    groups :: Core.Maybe [Core.Text],
    -- | The password of the user. This value must be at least 12 characters long, must contain at least 4 unique characters, and must not contain commas.
    password :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateUser' value with any optional fields omitted.
mkUpdateUser ::
  -- | 'username'
  Core.Text ->
  -- | 'brokerId'
  Core.Text ->
  UpdateUser
mkUpdateUser username brokerId =
  UpdateUser'
    { username,
      brokerId,
      consoleAccess = Core.Nothing,
      groups = Core.Nothing,
      password = Core.Nothing
    }

-- | Required. The username of the ActiveMQ user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuUsername :: Lens.Lens' UpdateUser Core.Text
uuUsername = Lens.field @"username"
{-# DEPRECATED uuUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The unique ID that Amazon MQ generates for the broker.
--
-- /Note:/ Consider using 'brokerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuBrokerId :: Lens.Lens' UpdateUser Core.Text
uuBrokerId = Lens.field @"brokerId"
{-# DEPRECATED uuBrokerId "Use generic-lens or generic-optics with 'brokerId' instead." #-}

-- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
--
-- /Note:/ Consider using 'consoleAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuConsoleAccess :: Lens.Lens' UpdateUser (Core.Maybe Core.Bool)
uuConsoleAccess = Lens.field @"consoleAccess"
{-# DEPRECATED uuConsoleAccess "Use generic-lens or generic-optics with 'consoleAccess' instead." #-}

-- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuGroups :: Lens.Lens' UpdateUser (Core.Maybe [Core.Text])
uuGroups = Lens.field @"groups"
{-# DEPRECATED uuGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | The password of the user. This value must be at least 12 characters long, must contain at least 4 unique characters, and must not contain commas.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuPassword :: Lens.Lens' UpdateUser (Core.Maybe Core.Text)
uuPassword = Lens.field @"password"
{-# DEPRECATED uuPassword "Use generic-lens or generic-optics with 'password' instead." #-}

instance Core.FromJSON UpdateUser where
  toJSON UpdateUser {..} =
    Core.object
      ( Core.catMaybes
          [ ("consoleAccess" Core..=) Core.<$> consoleAccess,
            ("groups" Core..=) Core.<$> groups,
            ("password" Core..=) Core.<$> password
          ]
      )

instance Core.AWSRequest UpdateUser where
  type Rs UpdateUser = UpdateUserResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/v1/brokers/" Core.<> (Core.toText brokerId) Core.<> ("/users/")
                Core.<> (Core.toText username)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateUserResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateUserResponse' smart constructor.
newtype UpdateUserResponse = UpdateUserResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateUserResponse' value with any optional fields omitted.
mkUpdateUserResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateUserResponse
mkUpdateUserResponse responseStatus =
  UpdateUserResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uurrsResponseStatus :: Lens.Lens' UpdateUserResponse Core.Int
uurrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uurrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
