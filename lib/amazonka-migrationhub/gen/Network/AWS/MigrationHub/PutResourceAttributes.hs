{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.PutResourceAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides identifying details of the resource being migrated so that it can be associated in the Application Discovery Service repository. This association occurs asynchronously after @PutResourceAttributes@ returns.
--
-- /Important:/
--     * Keep in mind that subsequent calls to PutResourceAttributes will override previously stored attributes. For example, if it is first called with a MAC address, but later, it is desired to /add/ an IP address, it will then be required to call it with /both/ the IP and MAC addresses to prevent overriding the MAC address.
--
--
--     * Note the instructions regarding the special use case of the <https://docs.aws.amazon.com/migrationhub/latest/ug/API_PutResourceAttributes.html#migrationhub-PutResourceAttributes-request-ResourceAttributeList @ResourceAttributeList@ > parameter when specifying any "VM" related value.
module Network.AWS.MigrationHub.PutResourceAttributes
  ( -- * Creating a request
    PutResourceAttributes (..),
    mkPutResourceAttributes,

    -- ** Request lenses
    praProgressUpdateStream,
    praMigrationTaskName,
    praResourceAttributeList,
    praDryRun,

    -- * Destructuring the response
    PutResourceAttributesResponse (..),
    mkPutResourceAttributesResponse,

    -- ** Response lenses
    prarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MigrationHub.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutResourceAttributes' smart constructor.
data PutResourceAttributes = PutResourceAttributes'
  { -- | The name of the ProgressUpdateStream.
    progressUpdateStream :: Types.ProgressUpdateStream,
    -- | Unique identifier that references the migration task. /Do not store personal data in this field./
    migrationTaskName :: Types.MigrationTaskName,
    -- | Information about the resource that is being migrated. This data will be used to map the task to a resource in the Application Discovery Service repository.
    --
    -- /Important:/
    --     * If any "VM" related value is set for a @ResourceAttribute@ object, it is required that @VM_MANAGER_ID@ , as a minimum, is always set. If @VM_MANAGER_ID@ is not set, then all "VM" fields will be discarded and "VM" fields will not be used for matching the migration task to a server in Application Discovery Service repository. See the <https://docs.aws.amazon.com/migrationhub/latest/ug/API_PutResourceAttributes.html#API_PutResourceAttributes_Examples Example> section below for a use case of specifying "VM" related values.
    --
    --
    --     * If a server you are trying to match has multiple IP or MAC addresses, you should provide as many as you know in separate type/value pairs passed to the @ResourceAttributeList@ parameter to maximize the chances of matching.
    resourceAttributeList :: Core.NonEmpty Types.ResourceAttribute,
    -- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutResourceAttributes' value with any optional fields omitted.
mkPutResourceAttributes ::
  -- | 'progressUpdateStream'
  Types.ProgressUpdateStream ->
  -- | 'migrationTaskName'
  Types.MigrationTaskName ->
  -- | 'resourceAttributeList'
  Core.NonEmpty Types.ResourceAttribute ->
  PutResourceAttributes
mkPutResourceAttributes
  progressUpdateStream
  migrationTaskName
  resourceAttributeList =
    PutResourceAttributes'
      { progressUpdateStream,
        migrationTaskName,
        resourceAttributeList,
        dryRun = Core.Nothing
      }

-- | The name of the ProgressUpdateStream.
--
-- /Note:/ Consider using 'progressUpdateStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
praProgressUpdateStream :: Lens.Lens' PutResourceAttributes Types.ProgressUpdateStream
praProgressUpdateStream = Lens.field @"progressUpdateStream"
{-# DEPRECATED praProgressUpdateStream "Use generic-lens or generic-optics with 'progressUpdateStream' instead." #-}

-- | Unique identifier that references the migration task. /Do not store personal data in this field./
--
-- /Note:/ Consider using 'migrationTaskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
praMigrationTaskName :: Lens.Lens' PutResourceAttributes Types.MigrationTaskName
praMigrationTaskName = Lens.field @"migrationTaskName"
{-# DEPRECATED praMigrationTaskName "Use generic-lens or generic-optics with 'migrationTaskName' instead." #-}

-- | Information about the resource that is being migrated. This data will be used to map the task to a resource in the Application Discovery Service repository.
--
-- /Important:/
--     * If any "VM" related value is set for a @ResourceAttribute@ object, it is required that @VM_MANAGER_ID@ , as a minimum, is always set. If @VM_MANAGER_ID@ is not set, then all "VM" fields will be discarded and "VM" fields will not be used for matching the migration task to a server in Application Discovery Service repository. See the <https://docs.aws.amazon.com/migrationhub/latest/ug/API_PutResourceAttributes.html#API_PutResourceAttributes_Examples Example> section below for a use case of specifying "VM" related values.
--
--
--     * If a server you are trying to match has multiple IP or MAC addresses, you should provide as many as you know in separate type/value pairs passed to the @ResourceAttributeList@ parameter to maximize the chances of matching.
--
--
--
-- /Note:/ Consider using 'resourceAttributeList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
praResourceAttributeList :: Lens.Lens' PutResourceAttributes (Core.NonEmpty Types.ResourceAttribute)
praResourceAttributeList = Lens.field @"resourceAttributeList"
{-# DEPRECATED praResourceAttributeList "Use generic-lens or generic-optics with 'resourceAttributeList' instead." #-}

-- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
praDryRun :: Lens.Lens' PutResourceAttributes (Core.Maybe Core.Bool)
praDryRun = Lens.field @"dryRun"
{-# DEPRECATED praDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.FromJSON PutResourceAttributes where
  toJSON PutResourceAttributes {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ProgressUpdateStream" Core..= progressUpdateStream),
            Core.Just ("MigrationTaskName" Core..= migrationTaskName),
            Core.Just ("ResourceAttributeList" Core..= resourceAttributeList),
            ("DryRun" Core..=) Core.<$> dryRun
          ]
      )

instance Core.AWSRequest PutResourceAttributes where
  type Rs PutResourceAttributes = PutResourceAttributesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSMigrationHub.PutResourceAttributes")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutResourceAttributesResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutResourceAttributesResponse' smart constructor.
newtype PutResourceAttributesResponse = PutResourceAttributesResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutResourceAttributesResponse' value with any optional fields omitted.
mkPutResourceAttributesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutResourceAttributesResponse
mkPutResourceAttributesResponse responseStatus =
  PutResourceAttributesResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prarrsResponseStatus :: Lens.Lens' PutResourceAttributesResponse Core.Int
prarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED prarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
