{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeOrganization (..)
    , mkDescribeOrganization
    -- ** Request lenses
    , dofOrganizationId

    -- * Destructuring the response
    , DescribeOrganizationResponse (..)
    , mkDescribeOrganizationResponse
    -- ** Response lenses
    , dorrsARN
    , dorrsAlias
    , dorrsCompletedDate
    , dorrsDefaultMailDomain
    , dorrsDirectoryId
    , dorrsDirectoryType
    , dorrsErrorMessage
    , dorrsOrganizationId
    , dorrsState
    , dorrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkDescribeOrganization' smart constructor.
newtype DescribeOrganization = DescribeOrganization'
  { organizationId :: Types.OrganizationId
    -- ^ The identifier for the organization to be described.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeOrganization' value with any optional fields omitted.
mkDescribeOrganization
    :: Types.OrganizationId -- ^ 'organizationId'
    -> DescribeOrganization
mkDescribeOrganization organizationId
  = DescribeOrganization'{organizationId}

-- | The identifier for the organization to be described.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dofOrganizationId :: Lens.Lens' DescribeOrganization Types.OrganizationId
dofOrganizationId = Lens.field @"organizationId"
{-# INLINEABLE dofOrganizationId #-}
{-# DEPRECATED organizationId "Use generic-lens or generic-optics with 'organizationId' instead"  #-}

instance Core.ToQuery DescribeOrganization where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeOrganization where
        toHeaders DescribeOrganization{..}
          = Core.pure
              ("X-Amz-Target", "WorkMailService.DescribeOrganization")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeOrganization where
        toJSON DescribeOrganization{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("OrganizationId" Core..= organizationId)])

instance Core.AWSRequest DescribeOrganization where
        type Rs DescribeOrganization = DescribeOrganizationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeOrganizationResponse' Core.<$>
                   (x Core..:? "ARN") Core.<*> x Core..:? "Alias" Core.<*>
                     x Core..:? "CompletedDate"
                     Core.<*> x Core..:? "DefaultMailDomain"
                     Core.<*> x Core..:? "DirectoryId"
                     Core.<*> x Core..:? "DirectoryType"
                     Core.<*> x Core..:? "ErrorMessage"
                     Core.<*> x Core..:? "OrganizationId"
                     Core.<*> x Core..:? "State"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeOrganizationResponse' smart constructor.
data DescribeOrganizationResponse = DescribeOrganizationResponse'
  { arn :: Core.Maybe Types.ARN
    -- ^ The Amazon Resource Name (ARN) of the organization.
  , alias :: Core.Maybe Types.OrganizationName
    -- ^ The alias for an organization.
  , completedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date at which the organization became usable in the WorkMail context, in UNIX epoch time format.
  , defaultMailDomain :: Core.Maybe Core.Text
    -- ^ The default mail domain associated with the organization.
  , directoryId :: Core.Maybe Core.Text
    -- ^ The identifier for the directory associated with an Amazon WorkMail organization.
  , directoryType :: Core.Maybe Core.Text
    -- ^ The type of directory associated with the WorkMail organization.
  , errorMessage :: Core.Maybe Core.Text
    -- ^ (Optional) The error message indicating if unexpected behavior was encountered with regards to the organization.
  , organizationId :: Core.Maybe Types.OrganizationId
    -- ^ The identifier of an organization.
  , state :: Core.Maybe Core.Text
    -- ^ The state of an organization.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeOrganizationResponse' value with any optional fields omitted.
mkDescribeOrganizationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeOrganizationResponse
mkDescribeOrganizationResponse responseStatus
  = DescribeOrganizationResponse'{arn = Core.Nothing,
                                  alias = Core.Nothing, completedDate = Core.Nothing,
                                  defaultMailDomain = Core.Nothing, directoryId = Core.Nothing,
                                  directoryType = Core.Nothing, errorMessage = Core.Nothing,
                                  organizationId = Core.Nothing, state = Core.Nothing,
                                  responseStatus}

-- | The Amazon Resource Name (ARN) of the organization.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorrsARN :: Lens.Lens' DescribeOrganizationResponse (Core.Maybe Types.ARN)
dorrsARN = Lens.field @"arn"
{-# INLINEABLE dorrsARN #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The alias for an organization.
--
-- /Note:/ Consider using 'alias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorrsAlias :: Lens.Lens' DescribeOrganizationResponse (Core.Maybe Types.OrganizationName)
dorrsAlias = Lens.field @"alias"
{-# INLINEABLE dorrsAlias #-}
{-# DEPRECATED alias "Use generic-lens or generic-optics with 'alias' instead"  #-}

-- | The date at which the organization became usable in the WorkMail context, in UNIX epoch time format.
--
-- /Note:/ Consider using 'completedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorrsCompletedDate :: Lens.Lens' DescribeOrganizationResponse (Core.Maybe Core.NominalDiffTime)
dorrsCompletedDate = Lens.field @"completedDate"
{-# INLINEABLE dorrsCompletedDate #-}
{-# DEPRECATED completedDate "Use generic-lens or generic-optics with 'completedDate' instead"  #-}

-- | The default mail domain associated with the organization.
--
-- /Note:/ Consider using 'defaultMailDomain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorrsDefaultMailDomain :: Lens.Lens' DescribeOrganizationResponse (Core.Maybe Core.Text)
dorrsDefaultMailDomain = Lens.field @"defaultMailDomain"
{-# INLINEABLE dorrsDefaultMailDomain #-}
{-# DEPRECATED defaultMailDomain "Use generic-lens or generic-optics with 'defaultMailDomain' instead"  #-}

-- | The identifier for the directory associated with an Amazon WorkMail organization.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorrsDirectoryId :: Lens.Lens' DescribeOrganizationResponse (Core.Maybe Core.Text)
dorrsDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE dorrsDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The type of directory associated with the WorkMail organization.
--
-- /Note:/ Consider using 'directoryType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorrsDirectoryType :: Lens.Lens' DescribeOrganizationResponse (Core.Maybe Core.Text)
dorrsDirectoryType = Lens.field @"directoryType"
{-# INLINEABLE dorrsDirectoryType #-}
{-# DEPRECATED directoryType "Use generic-lens or generic-optics with 'directoryType' instead"  #-}

-- | (Optional) The error message indicating if unexpected behavior was encountered with regards to the organization.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorrsErrorMessage :: Lens.Lens' DescribeOrganizationResponse (Core.Maybe Core.Text)
dorrsErrorMessage = Lens.field @"errorMessage"
{-# INLINEABLE dorrsErrorMessage #-}
{-# DEPRECATED errorMessage "Use generic-lens or generic-optics with 'errorMessage' instead"  #-}

-- | The identifier of an organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorrsOrganizationId :: Lens.Lens' DescribeOrganizationResponse (Core.Maybe Types.OrganizationId)
dorrsOrganizationId = Lens.field @"organizationId"
{-# INLINEABLE dorrsOrganizationId #-}
{-# DEPRECATED organizationId "Use generic-lens or generic-optics with 'organizationId' instead"  #-}

-- | The state of an organization.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorrsState :: Lens.Lens' DescribeOrganizationResponse (Core.Maybe Core.Text)
dorrsState = Lens.field @"state"
{-# INLINEABLE dorrsState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorrsResponseStatus :: Lens.Lens' DescribeOrganizationResponse Core.Int
dorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
