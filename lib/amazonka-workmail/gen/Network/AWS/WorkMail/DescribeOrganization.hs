{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.DescribeOrganization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides more information regarding a given organization based on its identifier.
module Network.AWS.WorkMail.DescribeOrganization
  ( -- * Creating a request
    DescribeOrganization (..),
    mkDescribeOrganization,

    -- ** Request lenses
    dofOrganizationId,

    -- * Destructuring the response
    DescribeOrganizationResponse (..),
    mkDescribeOrganizationResponse,

    -- ** Response lenses
    dorrsARN,
    dorrsAlias,
    dorrsCompletedDate,
    dorrsDefaultMailDomain,
    dorrsDirectoryId,
    dorrsDirectoryType,
    dorrsErrorMessage,
    dorrsOrganizationId,
    dorrsState,
    dorrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkDescribeOrganization' smart constructor.
newtype DescribeOrganization = DescribeOrganization'
  { -- | The identifier for the organization to be described.
    organizationId :: Types.OrganizationId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeOrganization' value with any optional fields omitted.
mkDescribeOrganization ::
  -- | 'organizationId'
  Types.OrganizationId ->
  DescribeOrganization
mkDescribeOrganization organizationId =
  DescribeOrganization' {organizationId}

-- | The identifier for the organization to be described.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dofOrganizationId :: Lens.Lens' DescribeOrganization Types.OrganizationId
dofOrganizationId = Lens.field @"organizationId"
{-# DEPRECATED dofOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Core.FromJSON DescribeOrganization where
  toJSON DescribeOrganization {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("OrganizationId" Core..= organizationId)]
      )

instance Core.AWSRequest DescribeOrganization where
  type Rs DescribeOrganization = DescribeOrganizationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "WorkMailService.DescribeOrganization")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeOrganizationResponse'
            Core.<$> (x Core..:? "ARN")
            Core.<*> (x Core..:? "Alias")
            Core.<*> (x Core..:? "CompletedDate")
            Core.<*> (x Core..:? "DefaultMailDomain")
            Core.<*> (x Core..:? "DirectoryId")
            Core.<*> (x Core..:? "DirectoryType")
            Core.<*> (x Core..:? "ErrorMessage")
            Core.<*> (x Core..:? "OrganizationId")
            Core.<*> (x Core..:? "State")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeOrganizationResponse' smart constructor.
data DescribeOrganizationResponse = DescribeOrganizationResponse'
  { -- | The Amazon Resource Name (ARN) of the organization.
    arn :: Core.Maybe Types.ARN,
    -- | The alias for an organization.
    alias :: Core.Maybe Types.OrganizationName,
    -- | The date at which the organization became usable in the WorkMail context, in UNIX epoch time format.
    completedDate :: Core.Maybe Core.NominalDiffTime,
    -- | The default mail domain associated with the organization.
    defaultMailDomain :: Core.Maybe Types.String,
    -- | The identifier for the directory associated with an Amazon WorkMail organization.
    directoryId :: Core.Maybe Types.String,
    -- | The type of directory associated with the WorkMail organization.
    directoryType :: Core.Maybe Types.String,
    -- | (Optional) The error message indicating if unexpected behavior was encountered with regards to the organization.
    errorMessage :: Core.Maybe Types.String,
    -- | The identifier of an organization.
    organizationId :: Core.Maybe Types.OrganizationId,
    -- | The state of an organization.
    state :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeOrganizationResponse' value with any optional fields omitted.
mkDescribeOrganizationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeOrganizationResponse
mkDescribeOrganizationResponse responseStatus =
  DescribeOrganizationResponse'
    { arn = Core.Nothing,
      alias = Core.Nothing,
      completedDate = Core.Nothing,
      defaultMailDomain = Core.Nothing,
      directoryId = Core.Nothing,
      directoryType = Core.Nothing,
      errorMessage = Core.Nothing,
      organizationId = Core.Nothing,
      state = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the organization.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorrsARN :: Lens.Lens' DescribeOrganizationResponse (Core.Maybe Types.ARN)
dorrsARN = Lens.field @"arn"
{-# DEPRECATED dorrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The alias for an organization.
--
-- /Note:/ Consider using 'alias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorrsAlias :: Lens.Lens' DescribeOrganizationResponse (Core.Maybe Types.OrganizationName)
dorrsAlias = Lens.field @"alias"
{-# DEPRECATED dorrsAlias "Use generic-lens or generic-optics with 'alias' instead." #-}

-- | The date at which the organization became usable in the WorkMail context, in UNIX epoch time format.
--
-- /Note:/ Consider using 'completedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorrsCompletedDate :: Lens.Lens' DescribeOrganizationResponse (Core.Maybe Core.NominalDiffTime)
dorrsCompletedDate = Lens.field @"completedDate"
{-# DEPRECATED dorrsCompletedDate "Use generic-lens or generic-optics with 'completedDate' instead." #-}

-- | The default mail domain associated with the organization.
--
-- /Note:/ Consider using 'defaultMailDomain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorrsDefaultMailDomain :: Lens.Lens' DescribeOrganizationResponse (Core.Maybe Types.String)
dorrsDefaultMailDomain = Lens.field @"defaultMailDomain"
{-# DEPRECATED dorrsDefaultMailDomain "Use generic-lens or generic-optics with 'defaultMailDomain' instead." #-}

-- | The identifier for the directory associated with an Amazon WorkMail organization.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorrsDirectoryId :: Lens.Lens' DescribeOrganizationResponse (Core.Maybe Types.String)
dorrsDirectoryId = Lens.field @"directoryId"
{-# DEPRECATED dorrsDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The type of directory associated with the WorkMail organization.
--
-- /Note:/ Consider using 'directoryType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorrsDirectoryType :: Lens.Lens' DescribeOrganizationResponse (Core.Maybe Types.String)
dorrsDirectoryType = Lens.field @"directoryType"
{-# DEPRECATED dorrsDirectoryType "Use generic-lens or generic-optics with 'directoryType' instead." #-}

-- | (Optional) The error message indicating if unexpected behavior was encountered with regards to the organization.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorrsErrorMessage :: Lens.Lens' DescribeOrganizationResponse (Core.Maybe Types.String)
dorrsErrorMessage = Lens.field @"errorMessage"
{-# DEPRECATED dorrsErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The identifier of an organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorrsOrganizationId :: Lens.Lens' DescribeOrganizationResponse (Core.Maybe Types.OrganizationId)
dorrsOrganizationId = Lens.field @"organizationId"
{-# DEPRECATED dorrsOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The state of an organization.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorrsState :: Lens.Lens' DescribeOrganizationResponse (Core.Maybe Types.String)
dorrsState = Lens.field @"state"
{-# DEPRECATED dorrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorrsResponseStatus :: Lens.Lens' DescribeOrganizationResponse Core.Int
dorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
