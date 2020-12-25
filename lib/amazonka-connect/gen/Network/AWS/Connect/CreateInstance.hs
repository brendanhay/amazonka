{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.CreateInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates an Amazon Connect instance with all the supported channels enabled. It does not attach any storage (such as Amazon S3, or Kinesis) or allow for any configurations on features such as Contact Lens for Amazon Connect.
module Network.AWS.Connect.CreateInstance
  ( -- * Creating a request
    CreateInstance (..),
    mkCreateInstance,

    -- ** Request lenses
    ciIdentityManagementType,
    ciInboundCallsEnabled,
    ciOutboundCallsEnabled,
    ciClientToken,
    ciDirectoryId,
    ciInstanceAlias,

    -- * Destructuring the response
    CreateInstanceResponse (..),
    mkCreateInstanceResponse,

    -- ** Response lenses
    cirrsArn,
    cirrsId,
    cirrsResponseStatus,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateInstance' smart constructor.
data CreateInstance = CreateInstance'
  { -- | The type of identity management for your Amazon Connect users.
    identityManagementType :: Types.DirectoryType,
    -- | Whether your contact center handles incoming contacts.
    inboundCallsEnabled :: Core.Bool,
    -- | Whether your contact center allows outbound calls.
    outboundCallsEnabled :: Core.Bool,
    -- | The idempotency token.
    clientToken :: Core.Maybe Types.ClientToken,
    -- | The identifier for the directory.
    directoryId :: Core.Maybe Types.DirectoryId,
    -- | The name for your instance.
    instanceAlias :: Core.Maybe Types.InstanceAlias
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateInstance' value with any optional fields omitted.
mkCreateInstance ::
  -- | 'identityManagementType'
  Types.DirectoryType ->
  -- | 'inboundCallsEnabled'
  Core.Bool ->
  -- | 'outboundCallsEnabled'
  Core.Bool ->
  CreateInstance
mkCreateInstance
  identityManagementType
  inboundCallsEnabled
  outboundCallsEnabled =
    CreateInstance'
      { identityManagementType,
        inboundCallsEnabled,
        outboundCallsEnabled,
        clientToken = Core.Nothing,
        directoryId = Core.Nothing,
        instanceAlias = Core.Nothing
      }

-- | The type of identity management for your Amazon Connect users.
--
-- /Note:/ Consider using 'identityManagementType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciIdentityManagementType :: Lens.Lens' CreateInstance Types.DirectoryType
ciIdentityManagementType = Lens.field @"identityManagementType"
{-# DEPRECATED ciIdentityManagementType "Use generic-lens or generic-optics with 'identityManagementType' instead." #-}

-- | Whether your contact center handles incoming contacts.
--
-- /Note:/ Consider using 'inboundCallsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciInboundCallsEnabled :: Lens.Lens' CreateInstance Core.Bool
ciInboundCallsEnabled = Lens.field @"inboundCallsEnabled"
{-# DEPRECATED ciInboundCallsEnabled "Use generic-lens or generic-optics with 'inboundCallsEnabled' instead." #-}

-- | Whether your contact center allows outbound calls.
--
-- /Note:/ Consider using 'outboundCallsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciOutboundCallsEnabled :: Lens.Lens' CreateInstance Core.Bool
ciOutboundCallsEnabled = Lens.field @"outboundCallsEnabled"
{-# DEPRECATED ciOutboundCallsEnabled "Use generic-lens or generic-optics with 'outboundCallsEnabled' instead." #-}

-- | The idempotency token.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciClientToken :: Lens.Lens' CreateInstance (Core.Maybe Types.ClientToken)
ciClientToken = Lens.field @"clientToken"
{-# DEPRECATED ciClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The identifier for the directory.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciDirectoryId :: Lens.Lens' CreateInstance (Core.Maybe Types.DirectoryId)
ciDirectoryId = Lens.field @"directoryId"
{-# DEPRECATED ciDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The name for your instance.
--
-- /Note:/ Consider using 'instanceAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciInstanceAlias :: Lens.Lens' CreateInstance (Core.Maybe Types.InstanceAlias)
ciInstanceAlias = Lens.field @"instanceAlias"
{-# DEPRECATED ciInstanceAlias "Use generic-lens or generic-optics with 'instanceAlias' instead." #-}

instance Core.FromJSON CreateInstance where
  toJSON CreateInstance {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("IdentityManagementType" Core..= identityManagementType),
            Core.Just ("InboundCallsEnabled" Core..= inboundCallsEnabled),
            Core.Just ("OutboundCallsEnabled" Core..= outboundCallsEnabled),
            ("ClientToken" Core..=) Core.<$> clientToken,
            ("DirectoryId" Core..=) Core.<$> directoryId,
            ("InstanceAlias" Core..=) Core.<$> instanceAlias
          ]
      )

instance Core.AWSRequest CreateInstance where
  type Rs CreateInstance = CreateInstanceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath = Core.rawPath "/instance",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateInstanceResponse'
            Core.<$> (x Core..:? "Arn")
            Core.<*> (x Core..:? "Id")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateInstanceResponse' smart constructor.
data CreateInstanceResponse = CreateInstanceResponse'
  { -- | The Amazon Resource Name (ARN) of the instance.
    arn :: Core.Maybe Types.ARN,
    -- | The identifier for the instance.
    id :: Core.Maybe Types.InstanceId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateInstanceResponse' value with any optional fields omitted.
mkCreateInstanceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateInstanceResponse
mkCreateInstanceResponse responseStatus =
  CreateInstanceResponse'
    { arn = Core.Nothing,
      id = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the instance.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirrsArn :: Lens.Lens' CreateInstanceResponse (Core.Maybe Types.ARN)
cirrsArn = Lens.field @"arn"
{-# DEPRECATED cirrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The identifier for the instance.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirrsId :: Lens.Lens' CreateInstanceResponse (Core.Maybe Types.InstanceId)
cirrsId = Lens.field @"id"
{-# DEPRECATED cirrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirrsResponseStatus :: Lens.Lens' CreateInstanceResponse Core.Int
cirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
