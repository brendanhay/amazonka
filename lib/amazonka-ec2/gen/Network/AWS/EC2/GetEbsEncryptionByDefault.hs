{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.GetEbsEncryptionByDefault
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes whether EBS encryption by default is enabled for your account in the current Region.
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.GetEbsEncryptionByDefault
    (
    -- * Creating a request
      GetEbsEncryptionByDefault (..)
    , mkGetEbsEncryptionByDefault
    -- ** Request lenses
    , geebdDryRun

    -- * Destructuring the response
    , GetEbsEncryptionByDefaultResponse (..)
    , mkGetEbsEncryptionByDefaultResponse
    -- ** Response lenses
    , geebdrrsEbsEncryptionByDefault
    , geebdrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetEbsEncryptionByDefault' smart constructor.
newtype GetEbsEncryptionByDefault = GetEbsEncryptionByDefault'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetEbsEncryptionByDefault' value with any optional fields omitted.
mkGetEbsEncryptionByDefault
    :: GetEbsEncryptionByDefault
mkGetEbsEncryptionByDefault
  = GetEbsEncryptionByDefault'{dryRun = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geebdDryRun :: Lens.Lens' GetEbsEncryptionByDefault (Core.Maybe Core.Bool)
geebdDryRun = Lens.field @"dryRun"
{-# INLINEABLE geebdDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery GetEbsEncryptionByDefault where
        toQuery GetEbsEncryptionByDefault{..}
          = Core.toQueryPair "Action"
              ("GetEbsEncryptionByDefault" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders GetEbsEncryptionByDefault where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetEbsEncryptionByDefault where
        type Rs GetEbsEncryptionByDefault =
             GetEbsEncryptionByDefaultResponse
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
                 GetEbsEncryptionByDefaultResponse' Core.<$>
                   (x Core..@? "ebsEncryptionByDefault") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetEbsEncryptionByDefaultResponse' smart constructor.
data GetEbsEncryptionByDefaultResponse = GetEbsEncryptionByDefaultResponse'
  { ebsEncryptionByDefault :: Core.Maybe Core.Bool
    -- ^ Indicates whether encryption by default is enabled.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetEbsEncryptionByDefaultResponse' value with any optional fields omitted.
mkGetEbsEncryptionByDefaultResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetEbsEncryptionByDefaultResponse
mkGetEbsEncryptionByDefaultResponse responseStatus
  = GetEbsEncryptionByDefaultResponse'{ebsEncryptionByDefault =
                                         Core.Nothing,
                                       responseStatus}

-- | Indicates whether encryption by default is enabled.
--
-- /Note:/ Consider using 'ebsEncryptionByDefault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geebdrrsEbsEncryptionByDefault :: Lens.Lens' GetEbsEncryptionByDefaultResponse (Core.Maybe Core.Bool)
geebdrrsEbsEncryptionByDefault = Lens.field @"ebsEncryptionByDefault"
{-# INLINEABLE geebdrrsEbsEncryptionByDefault #-}
{-# DEPRECATED ebsEncryptionByDefault "Use generic-lens or generic-optics with 'ebsEncryptionByDefault' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geebdrrsResponseStatus :: Lens.Lens' GetEbsEncryptionByDefaultResponse Core.Int
geebdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE geebdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
