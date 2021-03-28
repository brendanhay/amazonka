{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DisableEbsEncryptionByDefault
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables EBS encryption by default for your account in the current Region.
--
-- After you disable encryption by default, you can still create encrypted volumes by enabling encryption when you create each volume.
-- Disabling encryption by default does not change the encryption status of your existing volumes.
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.DisableEbsEncryptionByDefault
    (
    -- * Creating a request
      DisableEbsEncryptionByDefault (..)
    , mkDisableEbsEncryptionByDefault
    -- ** Request lenses
    , deebdDryRun

    -- * Destructuring the response
    , DisableEbsEncryptionByDefaultResponse (..)
    , mkDisableEbsEncryptionByDefaultResponse
    -- ** Response lenses
    , deebdrrsEbsEncryptionByDefault
    , deebdrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisableEbsEncryptionByDefault' smart constructor.
newtype DisableEbsEncryptionByDefault = DisableEbsEncryptionByDefault'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisableEbsEncryptionByDefault' value with any optional fields omitted.
mkDisableEbsEncryptionByDefault
    :: DisableEbsEncryptionByDefault
mkDisableEbsEncryptionByDefault
  = DisableEbsEncryptionByDefault'{dryRun = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deebdDryRun :: Lens.Lens' DisableEbsEncryptionByDefault (Core.Maybe Core.Bool)
deebdDryRun = Lens.field @"dryRun"
{-# INLINEABLE deebdDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DisableEbsEncryptionByDefault where
        toQuery DisableEbsEncryptionByDefault{..}
          = Core.toQueryPair "Action"
              ("DisableEbsEncryptionByDefault" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DisableEbsEncryptionByDefault where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DisableEbsEncryptionByDefault where
        type Rs DisableEbsEncryptionByDefault =
             DisableEbsEncryptionByDefaultResponse
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
                 DisableEbsEncryptionByDefaultResponse' Core.<$>
                   (x Core..@? "ebsEncryptionByDefault") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisableEbsEncryptionByDefaultResponse' smart constructor.
data DisableEbsEncryptionByDefaultResponse = DisableEbsEncryptionByDefaultResponse'
  { ebsEncryptionByDefault :: Core.Maybe Core.Bool
    -- ^ The updated status of encryption by default.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableEbsEncryptionByDefaultResponse' value with any optional fields omitted.
mkDisableEbsEncryptionByDefaultResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisableEbsEncryptionByDefaultResponse
mkDisableEbsEncryptionByDefaultResponse responseStatus
  = DisableEbsEncryptionByDefaultResponse'{ebsEncryptionByDefault =
                                             Core.Nothing,
                                           responseStatus}

-- | The updated status of encryption by default.
--
-- /Note:/ Consider using 'ebsEncryptionByDefault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deebdrrsEbsEncryptionByDefault :: Lens.Lens' DisableEbsEncryptionByDefaultResponse (Core.Maybe Core.Bool)
deebdrrsEbsEncryptionByDefault = Lens.field @"ebsEncryptionByDefault"
{-# INLINEABLE deebdrrsEbsEncryptionByDefault #-}
{-# DEPRECATED ebsEncryptionByDefault "Use generic-lens or generic-optics with 'ebsEncryptionByDefault' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deebdrrsResponseStatus :: Lens.Lens' DisableEbsEncryptionByDefaultResponse Core.Int
deebdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE deebdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
