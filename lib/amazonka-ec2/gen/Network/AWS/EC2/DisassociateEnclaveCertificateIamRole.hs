{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DisassociateEnclaveCertificateIamRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates an IAM role from an AWS Certificate Manager (ACM) certificate. Disassociating an IAM role from an ACM certificate removes the Amazon S3 object that contains the certificate, certificate chain, and encrypted private key from the Amazon S3 bucket. It also revokes the IAM role's permission to use the AWS Key Management Service (KMS) customer master key (CMK) used to encrypt the private key. This effectively revokes the role's permission to use the certificate. 
module Network.AWS.EC2.DisassociateEnclaveCertificateIamRole
    (
    -- * Creating a request
      DisassociateEnclaveCertificateIamRole (..)
    , mkDisassociateEnclaveCertificateIamRole
    -- ** Request lenses
    , decirCertificateArn
    , decirDryRun
    , decirRoleArn

    -- * Destructuring the response
    , DisassociateEnclaveCertificateIamRoleResponse (..)
    , mkDisassociateEnclaveCertificateIamRoleResponse
    -- ** Response lenses
    , decirrrsReturn
    , decirrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateEnclaveCertificateIamRole' smart constructor.
data DisassociateEnclaveCertificateIamRole = DisassociateEnclaveCertificateIamRole'
  { certificateArn :: Core.Maybe Types.CertificateArn
    -- ^ The ARN of the ACM certificate from which to disassociate the IAM role.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , roleArn :: Core.Maybe Types.RoleArn
    -- ^ The ARN of the IAM role to disassociate.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateEnclaveCertificateIamRole' value with any optional fields omitted.
mkDisassociateEnclaveCertificateIamRole
    :: DisassociateEnclaveCertificateIamRole
mkDisassociateEnclaveCertificateIamRole
  = DisassociateEnclaveCertificateIamRole'{certificateArn =
                                             Core.Nothing,
                                           dryRun = Core.Nothing, roleArn = Core.Nothing}

-- | The ARN of the ACM certificate from which to disassociate the IAM role.
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decirCertificateArn :: Lens.Lens' DisassociateEnclaveCertificateIamRole (Core.Maybe Types.CertificateArn)
decirCertificateArn = Lens.field @"certificateArn"
{-# INLINEABLE decirCertificateArn #-}
{-# DEPRECATED certificateArn "Use generic-lens or generic-optics with 'certificateArn' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decirDryRun :: Lens.Lens' DisassociateEnclaveCertificateIamRole (Core.Maybe Core.Bool)
decirDryRun = Lens.field @"dryRun"
{-# INLINEABLE decirDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The ARN of the IAM role to disassociate.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decirRoleArn :: Lens.Lens' DisassociateEnclaveCertificateIamRole (Core.Maybe Types.RoleArn)
decirRoleArn = Lens.field @"roleArn"
{-# INLINEABLE decirRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

instance Core.ToQuery DisassociateEnclaveCertificateIamRole where
        toQuery DisassociateEnclaveCertificateIamRole{..}
          = Core.toQueryPair "Action"
              ("DisassociateEnclaveCertificateIamRole" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CertificateArn")
                certificateArn
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "RoleArn") roleArn

instance Core.ToHeaders DisassociateEnclaveCertificateIamRole where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DisassociateEnclaveCertificateIamRole
         where
        type Rs DisassociateEnclaveCertificateIamRole =
             DisassociateEnclaveCertificateIamRoleResponse
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
                 DisassociateEnclaveCertificateIamRoleResponse' Core.<$>
                   (x Core..@? "return") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisassociateEnclaveCertificateIamRoleResponse' smart constructor.
data DisassociateEnclaveCertificateIamRoleResponse = DisassociateEnclaveCertificateIamRoleResponse'
  { return :: Core.Maybe Core.Bool
    -- ^ Returns @true@ if the request succeeds; otherwise, it returns an error.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateEnclaveCertificateIamRoleResponse' value with any optional fields omitted.
mkDisassociateEnclaveCertificateIamRoleResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisassociateEnclaveCertificateIamRoleResponse
mkDisassociateEnclaveCertificateIamRoleResponse responseStatus
  = DisassociateEnclaveCertificateIamRoleResponse'{return =
                                                     Core.Nothing,
                                                   responseStatus}

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decirrrsReturn :: Lens.Lens' DisassociateEnclaveCertificateIamRoleResponse (Core.Maybe Core.Bool)
decirrrsReturn = Lens.field @"return"
{-# INLINEABLE decirrrsReturn #-}
{-# DEPRECATED return "Use generic-lens or generic-optics with 'return' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decirrrsResponseStatus :: Lens.Lens' DisassociateEnclaveCertificateIamRoleResponse Core.Int
decirrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE decirrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
