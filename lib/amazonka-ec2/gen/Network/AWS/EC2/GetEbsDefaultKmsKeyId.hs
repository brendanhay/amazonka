{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.GetEbsDefaultKmsKeyId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the default customer master key (CMK) for EBS encryption by default for your account in this Region. You can change the default CMK for encryption by default using 'ModifyEbsDefaultKmsKeyId' or 'ResetEbsDefaultKmsKeyId' .
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.GetEbsDefaultKmsKeyId
    (
    -- * Creating a request
      GetEbsDefaultKmsKeyId (..)
    , mkGetEbsDefaultKmsKeyId
    -- ** Request lenses
    , gedkkiDryRun

    -- * Destructuring the response
    , GetEbsDefaultKmsKeyIdResponse (..)
    , mkGetEbsDefaultKmsKeyIdResponse
    -- ** Response lenses
    , gedkkirrsKmsKeyId
    , gedkkirrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetEbsDefaultKmsKeyId' smart constructor.
newtype GetEbsDefaultKmsKeyId = GetEbsDefaultKmsKeyId'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetEbsDefaultKmsKeyId' value with any optional fields omitted.
mkGetEbsDefaultKmsKeyId
    :: GetEbsDefaultKmsKeyId
mkGetEbsDefaultKmsKeyId
  = GetEbsDefaultKmsKeyId'{dryRun = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gedkkiDryRun :: Lens.Lens' GetEbsDefaultKmsKeyId (Core.Maybe Core.Bool)
gedkkiDryRun = Lens.field @"dryRun"
{-# INLINEABLE gedkkiDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery GetEbsDefaultKmsKeyId where
        toQuery GetEbsDefaultKmsKeyId{..}
          = Core.toQueryPair "Action" ("GetEbsDefaultKmsKeyId" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders GetEbsDefaultKmsKeyId where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetEbsDefaultKmsKeyId where
        type Rs GetEbsDefaultKmsKeyId = GetEbsDefaultKmsKeyIdResponse
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
                 GetEbsDefaultKmsKeyIdResponse' Core.<$>
                   (x Core..@? "kmsKeyId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetEbsDefaultKmsKeyIdResponse' smart constructor.
data GetEbsDefaultKmsKeyIdResponse = GetEbsDefaultKmsKeyIdResponse'
  { kmsKeyId :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the default CMK for encryption by default.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetEbsDefaultKmsKeyIdResponse' value with any optional fields omitted.
mkGetEbsDefaultKmsKeyIdResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetEbsDefaultKmsKeyIdResponse
mkGetEbsDefaultKmsKeyIdResponse responseStatus
  = GetEbsDefaultKmsKeyIdResponse'{kmsKeyId = Core.Nothing,
                                   responseStatus}

-- | The Amazon Resource Name (ARN) of the default CMK for encryption by default.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gedkkirrsKmsKeyId :: Lens.Lens' GetEbsDefaultKmsKeyIdResponse (Core.Maybe Core.Text)
gedkkirrsKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE gedkkirrsKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gedkkirrsResponseStatus :: Lens.Lens' GetEbsDefaultKmsKeyIdResponse Core.Int
gedkkirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gedkkirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
