{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.CreateOrganization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS organization. The account whose user is calling the @CreateOrganization@ operation automatically becomes the <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_getting-started_concepts.html#account management account> of the new organization.
--
-- This operation must be called using credentials from the account that is to become the new organization's management account. The principal must also have the relevant IAM permissions.
-- By default (or if you set the @FeatureSet@ parameter to @ALL@ ), the new organization is created with all features enabled and service control policies automatically enabled in the root. If you instead choose to create the organization supporting only the consolidated billing features by setting the @FeatureSet@ parameter to @CONSOLIDATED_BILLING"@ , no policy types are enabled by default, and you can't use organization policies
module Network.AWS.Organizations.CreateOrganization
    (
    -- * Creating a request
      CreateOrganization (..)
    , mkCreateOrganization
    -- ** Request lenses
    , coFeatureSet

    -- * Destructuring the response
    , CreateOrganizationResponse (..)
    , mkCreateOrganizationResponse
    -- ** Response lenses
    , corrsOrganization
    , corrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateOrganization' smart constructor.
newtype CreateOrganization = CreateOrganization'
  { featureSet :: Core.Maybe Types.OrganizationFeatureSet
    -- ^ Specifies the feature set supported by the new organization. Each feature set supports different levels of functionality.
--
--
--     * @CONSOLIDATED_BILLING@ : All member accounts have their bills consolidated to and paid by the management account. For more information, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_getting-started_concepts.html#feature-set-cb-only Consolidated billing> in the /AWS Organizations User Guide./ 
-- The consolidated billing feature subset isn't available for organizations in the AWS GovCloud (US) Region.
--
--
--     * @ALL@ : In addition to all the features supported by the consolidated billing feature set, the management account can also apply any policy type to any member account in the organization. For more information, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_getting-started_concepts.html#feature-set-all All features> in the /AWS Organizations User Guide./ 
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateOrganization' value with any optional fields omitted.
mkCreateOrganization
    :: CreateOrganization
mkCreateOrganization
  = CreateOrganization'{featureSet = Core.Nothing}

-- | Specifies the feature set supported by the new organization. Each feature set supports different levels of functionality.
--
--
--     * @CONSOLIDATED_BILLING@ : All member accounts have their bills consolidated to and paid by the management account. For more information, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_getting-started_concepts.html#feature-set-cb-only Consolidated billing> in the /AWS Organizations User Guide./ 
-- The consolidated billing feature subset isn't available for organizations in the AWS GovCloud (US) Region.
--
--
--     * @ALL@ : In addition to all the features supported by the consolidated billing feature set, the management account can also apply any policy type to any member account in the organization. For more information, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_getting-started_concepts.html#feature-set-all All features> in the /AWS Organizations User Guide./ 
--
--
--
-- /Note:/ Consider using 'featureSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coFeatureSet :: Lens.Lens' CreateOrganization (Core.Maybe Types.OrganizationFeatureSet)
coFeatureSet = Lens.field @"featureSet"
{-# INLINEABLE coFeatureSet #-}
{-# DEPRECATED featureSet "Use generic-lens or generic-optics with 'featureSet' instead"  #-}

instance Core.ToQuery CreateOrganization where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateOrganization where
        toHeaders CreateOrganization{..}
          = Core.pure
              ("X-Amz-Target", "AWSOrganizationsV20161128.CreateOrganization")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateOrganization where
        toJSON CreateOrganization{..}
          = Core.object
              (Core.catMaybes [("FeatureSet" Core..=) Core.<$> featureSet])

instance Core.AWSRequest CreateOrganization where
        type Rs CreateOrganization = CreateOrganizationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateOrganizationResponse' Core.<$>
                   (x Core..:? "Organization") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateOrganizationResponse' smart constructor.
data CreateOrganizationResponse = CreateOrganizationResponse'
  { organization :: Core.Maybe Types.Organization
    -- ^ A structure that contains details about the newly created organization.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateOrganizationResponse' value with any optional fields omitted.
mkCreateOrganizationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateOrganizationResponse
mkCreateOrganizationResponse responseStatus
  = CreateOrganizationResponse'{organization = Core.Nothing,
                                responseStatus}

-- | A structure that contains details about the newly created organization.
--
-- /Note:/ Consider using 'organization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corrsOrganization :: Lens.Lens' CreateOrganizationResponse (Core.Maybe Types.Organization)
corrsOrganization = Lens.field @"organization"
{-# INLINEABLE corrsOrganization #-}
{-# DEPRECATED organization "Use generic-lens or generic-optics with 'organization' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corrsResponseStatus :: Lens.Lens' CreateOrganizationResponse Core.Int
corrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE corrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
