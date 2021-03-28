{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.CreateOrganization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon WorkMail organization. Optionally, you can choose to associate an existing AWS Directory Service directory with your organization. If an AWS Directory Service directory ID is specified, the organization alias must match the directory alias. If you choose not to associate an existing directory with your organization, then we create a new Amazon WorkMail directory for you. For more information, see <https://docs.aws.amazon.com/workmail/latest/adminguide/add_new_organization.html Adding an organization> in the /Amazon WorkMail Administrator Guide/ .
--
-- You can associate multiple email domains with an organization, then set your default email domain from the Amazon WorkMail console. You can also associate a domain that is managed in an Amazon Route 53 public hosted zone. For more information, see <https://docs.aws.amazon.com/workmail/latest/adminguide/add_domain.html Adding a domain> and <https://docs.aws.amazon.com/workmail/latest/adminguide/default_domain.html Choosing the default domain> in the /Amazon WorkMail Administrator Guide/ .
-- Optionally, you can use a customer managed master key from AWS Key Management Service (AWS KMS) to encrypt email for your organization. If you don't associate an AWS KMS key, Amazon WorkMail creates a default AWS managed master key for you.
module Network.AWS.WorkMail.CreateOrganization
    (
    -- * Creating a request
      CreateOrganization (..)
    , mkCreateOrganization
    -- ** Request lenses
    , coAlias
    , coClientToken
    , coDirectoryId
    , coDomains
    , coEnableInteroperability
    , coKmsKeyArn

    -- * Destructuring the response
    , CreateOrganizationResponse (..)
    , mkCreateOrganizationResponse
    -- ** Response lenses
    , corrsOrganizationId
    , corrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkCreateOrganization' smart constructor.
data CreateOrganization = CreateOrganization'
  { alias :: Types.OrganizationName
    -- ^ The organization alias.
  , clientToken :: Core.Maybe Types.IdempotencyClientToken
    -- ^ The idempotency token associated with the request.
  , directoryId :: Core.Maybe Types.DirectoryId
    -- ^ The AWS Directory Service directory ID.
  , domains :: Core.Maybe [Types.Domain]
    -- ^ The email domains to associate with the organization.
  , enableInteroperability :: Core.Maybe Core.Bool
    -- ^ When @true@ , allows organization interoperability between Amazon WorkMail and Microsoft Exchange. Can only be set to @true@ if an AD Connector directory ID is included in the request.
  , kmsKeyArn :: Core.Maybe Types.KmsKeyArn
    -- ^ The Amazon Resource Name (ARN) of a customer managed master key from AWS KMS.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateOrganization' value with any optional fields omitted.
mkCreateOrganization
    :: Types.OrganizationName -- ^ 'alias'
    -> CreateOrganization
mkCreateOrganization alias
  = CreateOrganization'{alias, clientToken = Core.Nothing,
                        directoryId = Core.Nothing, domains = Core.Nothing,
                        enableInteroperability = Core.Nothing, kmsKeyArn = Core.Nothing}

-- | The organization alias.
--
-- /Note:/ Consider using 'alias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coAlias :: Lens.Lens' CreateOrganization Types.OrganizationName
coAlias = Lens.field @"alias"
{-# INLINEABLE coAlias #-}
{-# DEPRECATED alias "Use generic-lens or generic-optics with 'alias' instead"  #-}

-- | The idempotency token associated with the request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coClientToken :: Lens.Lens' CreateOrganization (Core.Maybe Types.IdempotencyClientToken)
coClientToken = Lens.field @"clientToken"
{-# INLINEABLE coClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | The AWS Directory Service directory ID.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coDirectoryId :: Lens.Lens' CreateOrganization (Core.Maybe Types.DirectoryId)
coDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE coDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The email domains to associate with the organization.
--
-- /Note:/ Consider using 'domains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coDomains :: Lens.Lens' CreateOrganization (Core.Maybe [Types.Domain])
coDomains = Lens.field @"domains"
{-# INLINEABLE coDomains #-}
{-# DEPRECATED domains "Use generic-lens or generic-optics with 'domains' instead"  #-}

-- | When @true@ , allows organization interoperability between Amazon WorkMail and Microsoft Exchange. Can only be set to @true@ if an AD Connector directory ID is included in the request.
--
-- /Note:/ Consider using 'enableInteroperability' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coEnableInteroperability :: Lens.Lens' CreateOrganization (Core.Maybe Core.Bool)
coEnableInteroperability = Lens.field @"enableInteroperability"
{-# INLINEABLE coEnableInteroperability #-}
{-# DEPRECATED enableInteroperability "Use generic-lens or generic-optics with 'enableInteroperability' instead"  #-}

-- | The Amazon Resource Name (ARN) of a customer managed master key from AWS KMS.
--
-- /Note:/ Consider using 'kmsKeyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coKmsKeyArn :: Lens.Lens' CreateOrganization (Core.Maybe Types.KmsKeyArn)
coKmsKeyArn = Lens.field @"kmsKeyArn"
{-# INLINEABLE coKmsKeyArn #-}
{-# DEPRECATED kmsKeyArn "Use generic-lens or generic-optics with 'kmsKeyArn' instead"  #-}

instance Core.ToQuery CreateOrganization where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateOrganization where
        toHeaders CreateOrganization{..}
          = Core.pure ("X-Amz-Target", "WorkMailService.CreateOrganization")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateOrganization where
        toJSON CreateOrganization{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Alias" Core..= alias),
                  ("ClientToken" Core..=) Core.<$> clientToken,
                  ("DirectoryId" Core..=) Core.<$> directoryId,
                  ("Domains" Core..=) Core.<$> domains,
                  ("EnableInteroperability" Core..=) Core.<$> enableInteroperability,
                  ("KmsKeyArn" Core..=) Core.<$> kmsKeyArn])

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
                   (x Core..:? "OrganizationId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateOrganizationResponse' smart constructor.
data CreateOrganizationResponse = CreateOrganizationResponse'
  { organizationId :: Core.Maybe Types.OrganizationId
    -- ^ The organization ID.
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
  = CreateOrganizationResponse'{organizationId = Core.Nothing,
                                responseStatus}

-- | The organization ID.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corrsOrganizationId :: Lens.Lens' CreateOrganizationResponse (Core.Maybe Types.OrganizationId)
corrsOrganizationId = Lens.field @"organizationId"
{-# INLINEABLE corrsOrganizationId #-}
{-# DEPRECATED organizationId "Use generic-lens or generic-optics with 'organizationId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corrsResponseStatus :: Lens.Lens' CreateOrganizationResponse Core.Int
corrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE corrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
