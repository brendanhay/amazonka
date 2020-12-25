{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.StopContact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Ends the specified contact.
module Network.AWS.Connect.StopContact
  ( -- * Creating a request
    StopContact (..),
    mkStopContact,

    -- ** Request lenses
    scContactId,
    scInstanceId,

    -- * Destructuring the response
    StopContactResponse (..),
    mkStopContactResponse,

    -- ** Response lenses
    scrrsResponseStatus,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopContact' smart constructor.
data StopContact = StopContact'
  { -- | The ID of the contact.
    contactId :: Types.ContactId,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopContact' value with any optional fields omitted.
mkStopContact ::
  -- | 'contactId'
  Types.ContactId ->
  -- | 'instanceId'
  Types.InstanceId ->
  StopContact
mkStopContact contactId instanceId =
  StopContact' {contactId, instanceId}

-- | The ID of the contact.
--
-- /Note:/ Consider using 'contactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scContactId :: Lens.Lens' StopContact Types.ContactId
scContactId = Lens.field @"contactId"
{-# DEPRECATED scContactId "Use generic-lens or generic-optics with 'contactId' instead." #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scInstanceId :: Lens.Lens' StopContact Types.InstanceId
scInstanceId = Lens.field @"instanceId"
{-# DEPRECATED scInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Core.FromJSON StopContact where
  toJSON StopContact {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ContactId" Core..= contactId),
            Core.Just ("InstanceId" Core..= instanceId)
          ]
      )

instance Core.AWSRequest StopContact where
  type Rs StopContact = StopContactResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/contact/stop",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopContactResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStopContactResponse' smart constructor.
newtype StopContactResponse = StopContactResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopContactResponse' value with any optional fields omitted.
mkStopContactResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StopContactResponse
mkStopContactResponse responseStatus =
  StopContactResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrrsResponseStatus :: Lens.Lens' StopContactResponse Core.Int
scrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED scrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
