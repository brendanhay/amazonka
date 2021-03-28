{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.GetAssociatedEnclaveCertificateIamRoles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the IAM roles that are associated with the specified AWS Certificate Manager (ACM) certificate. It also returns the name of the Amazon S3 bucket and the Amazon S3 object key where the certificate, certificate chain, and encrypted private key bundle are stored, and the ARN of the AWS Key Management Service (KMS) customer master key (CMK) that's used to encrypt the private key.
module Network.AWS.EC2.GetAssociatedEnclaveCertificateIamRoles
    (
    -- * Creating a request
      GetAssociatedEnclaveCertificateIamRoles (..)
    , mkGetAssociatedEnclaveCertificateIamRoles
    -- ** Request lenses
    , gaecirCertificateArn
    , gaecirDryRun

    -- * Destructuring the response
    , GetAssociatedEnclaveCertificateIamRolesResponse (..)
    , mkGetAssociatedEnclaveCertificateIamRolesResponse
    -- ** Response lenses
    , gaecirrrsAssociatedRoles
    , gaecirrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetAssociatedEnclaveCertificateIamRoles' smart constructor.
data GetAssociatedEnclaveCertificateIamRoles = GetAssociatedEnclaveCertificateIamRoles'
  { certificateArn :: Core.Maybe Types.ResourceArn
    -- ^ The ARN of the ACM certificate for which to view the associated IAM roles, encryption keys, and Amazon S3 object information.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAssociatedEnclaveCertificateIamRoles' value with any optional fields omitted.
mkGetAssociatedEnclaveCertificateIamRoles
    :: GetAssociatedEnclaveCertificateIamRoles
mkGetAssociatedEnclaveCertificateIamRoles
  = GetAssociatedEnclaveCertificateIamRoles'{certificateArn =
                                               Core.Nothing,
                                             dryRun = Core.Nothing}

-- | The ARN of the ACM certificate for which to view the associated IAM roles, encryption keys, and Amazon S3 object information.
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaecirCertificateArn :: Lens.Lens' GetAssociatedEnclaveCertificateIamRoles (Core.Maybe Types.ResourceArn)
gaecirCertificateArn = Lens.field @"certificateArn"
{-# INLINEABLE gaecirCertificateArn #-}
{-# DEPRECATED certificateArn "Use generic-lens or generic-optics with 'certificateArn' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaecirDryRun :: Lens.Lens' GetAssociatedEnclaveCertificateIamRoles (Core.Maybe Core.Bool)
gaecirDryRun = Lens.field @"dryRun"
{-# INLINEABLE gaecirDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery GetAssociatedEnclaveCertificateIamRoles where
        toQuery GetAssociatedEnclaveCertificateIamRoles{..}
          = Core.toQueryPair "Action"
              ("GetAssociatedEnclaveCertificateIamRoles" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CertificateArn")
                certificateArn
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders GetAssociatedEnclaveCertificateIamRoles
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetAssociatedEnclaveCertificateIamRoles
         where
        type Rs GetAssociatedEnclaveCertificateIamRoles =
             GetAssociatedEnclaveCertificateIamRolesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 GetAssociatedEnclaveCertificateIamRolesResponse' Core.<$>
                   (x Core..@? "associatedRoleSet" Core..<@> Core.parseXMLList "item")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetAssociatedEnclaveCertificateIamRolesResponse' smart constructor.
data GetAssociatedEnclaveCertificateIamRolesResponse = GetAssociatedEnclaveCertificateIamRolesResponse'
  { associatedRoles :: Core.Maybe [Types.AssociatedRole]
    -- ^ Information about the associated IAM roles.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAssociatedEnclaveCertificateIamRolesResponse' value with any optional fields omitted.
mkGetAssociatedEnclaveCertificateIamRolesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetAssociatedEnclaveCertificateIamRolesResponse
mkGetAssociatedEnclaveCertificateIamRolesResponse responseStatus
  = GetAssociatedEnclaveCertificateIamRolesResponse'{associatedRoles
                                                       = Core.Nothing,
                                                     responseStatus}

-- | Information about the associated IAM roles.
--
-- /Note:/ Consider using 'associatedRoles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaecirrrsAssociatedRoles :: Lens.Lens' GetAssociatedEnclaveCertificateIamRolesResponse (Core.Maybe [Types.AssociatedRole])
gaecirrrsAssociatedRoles = Lens.field @"associatedRoles"
{-# INLINEABLE gaecirrrsAssociatedRoles #-}
{-# DEPRECATED associatedRoles "Use generic-lens or generic-optics with 'associatedRoles' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaecirrrsResponseStatus :: Lens.Lens' GetAssociatedEnclaveCertificateIamRolesResponse Core.Int
gaecirrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gaecirrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
