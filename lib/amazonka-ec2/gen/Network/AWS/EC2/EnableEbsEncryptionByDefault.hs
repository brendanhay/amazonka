{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.EnableEbsEncryptionByDefault
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables EBS encryption by default for your account in the current Region.
--
-- After you enable encryption by default, the EBS volumes that you create are are always encrypted, either using the default CMK or the CMK that you specified when you created each volume. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
-- You can specify the default CMK for encryption by default using 'ModifyEbsDefaultKmsKeyId' or 'ResetEbsDefaultKmsKeyId' .
-- Enabling encryption by default has no effect on the encryption status of your existing volumes.
-- After you enable encryption by default, you can no longer launch instances using instance types that do not support encryption. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#EBSEncryption_supported_instances Supported instance types> .
module Network.AWS.EC2.EnableEbsEncryptionByDefault
    (
    -- * Creating a request
      EnableEbsEncryptionByDefault (..)
    , mkEnableEbsEncryptionByDefault
    -- ** Request lenses
    , eeebdDryRun

    -- * Destructuring the response
    , EnableEbsEncryptionByDefaultResponse (..)
    , mkEnableEbsEncryptionByDefaultResponse
    -- ** Response lenses
    , eeebdrrsEbsEncryptionByDefault
    , eeebdrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkEnableEbsEncryptionByDefault' smart constructor.
newtype EnableEbsEncryptionByDefault = EnableEbsEncryptionByDefault'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EnableEbsEncryptionByDefault' value with any optional fields omitted.
mkEnableEbsEncryptionByDefault
    :: EnableEbsEncryptionByDefault
mkEnableEbsEncryptionByDefault
  = EnableEbsEncryptionByDefault'{dryRun = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eeebdDryRun :: Lens.Lens' EnableEbsEncryptionByDefault (Core.Maybe Core.Bool)
eeebdDryRun = Lens.field @"dryRun"
{-# INLINEABLE eeebdDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery EnableEbsEncryptionByDefault where
        toQuery EnableEbsEncryptionByDefault{..}
          = Core.toQueryPair "Action"
              ("EnableEbsEncryptionByDefault" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders EnableEbsEncryptionByDefault where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest EnableEbsEncryptionByDefault where
        type Rs EnableEbsEncryptionByDefault =
             EnableEbsEncryptionByDefaultResponse
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
                 EnableEbsEncryptionByDefaultResponse' Core.<$>
                   (x Core..@? "ebsEncryptionByDefault") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkEnableEbsEncryptionByDefaultResponse' smart constructor.
data EnableEbsEncryptionByDefaultResponse = EnableEbsEncryptionByDefaultResponse'
  { ebsEncryptionByDefault :: Core.Maybe Core.Bool
    -- ^ The updated status of encryption by default.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableEbsEncryptionByDefaultResponse' value with any optional fields omitted.
mkEnableEbsEncryptionByDefaultResponse
    :: Core.Int -- ^ 'responseStatus'
    -> EnableEbsEncryptionByDefaultResponse
mkEnableEbsEncryptionByDefaultResponse responseStatus
  = EnableEbsEncryptionByDefaultResponse'{ebsEncryptionByDefault =
                                            Core.Nothing,
                                          responseStatus}

-- | The updated status of encryption by default.
--
-- /Note:/ Consider using 'ebsEncryptionByDefault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eeebdrrsEbsEncryptionByDefault :: Lens.Lens' EnableEbsEncryptionByDefaultResponse (Core.Maybe Core.Bool)
eeebdrrsEbsEncryptionByDefault = Lens.field @"ebsEncryptionByDefault"
{-# INLINEABLE eeebdrrsEbsEncryptionByDefault #-}
{-# DEPRECATED ebsEncryptionByDefault "Use generic-lens or generic-optics with 'ebsEncryptionByDefault' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eeebdrrsResponseStatus :: Lens.Lens' EnableEbsEncryptionByDefaultResponse Core.Int
eeebdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE eeebdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
