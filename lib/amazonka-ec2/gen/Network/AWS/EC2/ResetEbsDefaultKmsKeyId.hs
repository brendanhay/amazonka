{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ResetEbsDefaultKmsKeyId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets the default customer master key (CMK) for EBS encryption for your account in this Region to the AWS managed CMK for EBS.
--
-- After resetting the default CMK to the AWS managed CMK, you can continue to encrypt by a customer managed CMK by specifying it when you create the volume. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.ResetEbsDefaultKmsKeyId
    (
    -- * Creating a request
      ResetEbsDefaultKmsKeyId (..)
    , mkResetEbsDefaultKmsKeyId
    -- ** Request lenses
    , redkkiDryRun

    -- * Destructuring the response
    , ResetEbsDefaultKmsKeyIdResponse (..)
    , mkResetEbsDefaultKmsKeyIdResponse
    -- ** Response lenses
    , redkkirrsKmsKeyId
    , redkkirrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkResetEbsDefaultKmsKeyId' smart constructor.
newtype ResetEbsDefaultKmsKeyId = ResetEbsDefaultKmsKeyId'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ResetEbsDefaultKmsKeyId' value with any optional fields omitted.
mkResetEbsDefaultKmsKeyId
    :: ResetEbsDefaultKmsKeyId
mkResetEbsDefaultKmsKeyId
  = ResetEbsDefaultKmsKeyId'{dryRun = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
redkkiDryRun :: Lens.Lens' ResetEbsDefaultKmsKeyId (Core.Maybe Core.Bool)
redkkiDryRun = Lens.field @"dryRun"
{-# INLINEABLE redkkiDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery ResetEbsDefaultKmsKeyId where
        toQuery ResetEbsDefaultKmsKeyId{..}
          = Core.toQueryPair "Action"
              ("ResetEbsDefaultKmsKeyId" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders ResetEbsDefaultKmsKeyId where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ResetEbsDefaultKmsKeyId where
        type Rs ResetEbsDefaultKmsKeyId = ResetEbsDefaultKmsKeyIdResponse
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
                 ResetEbsDefaultKmsKeyIdResponse' Core.<$>
                   (x Core..@? "kmsKeyId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkResetEbsDefaultKmsKeyIdResponse' smart constructor.
data ResetEbsDefaultKmsKeyIdResponse = ResetEbsDefaultKmsKeyIdResponse'
  { kmsKeyId :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the default CMK for EBS encryption by default.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResetEbsDefaultKmsKeyIdResponse' value with any optional fields omitted.
mkResetEbsDefaultKmsKeyIdResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ResetEbsDefaultKmsKeyIdResponse
mkResetEbsDefaultKmsKeyIdResponse responseStatus
  = ResetEbsDefaultKmsKeyIdResponse'{kmsKeyId = Core.Nothing,
                                     responseStatus}

-- | The Amazon Resource Name (ARN) of the default CMK for EBS encryption by default.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
redkkirrsKmsKeyId :: Lens.Lens' ResetEbsDefaultKmsKeyIdResponse (Core.Maybe Core.Text)
redkkirrsKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE redkkirrsKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
redkkirrsResponseStatus :: Lens.Lens' ResetEbsDefaultKmsKeyIdResponse Core.Int
redkkirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE redkkirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
