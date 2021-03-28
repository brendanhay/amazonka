{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.GetPasswordData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the encrypted administrator password for a running Windows instance.
--
-- The Windows password is generated at boot by the @EC2Config@ service or @EC2Launch@ scripts (Windows Server 2016 and later). This usually only happens the first time an instance is launched. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/UsingConfig_WinAMI.html EC2Config> and <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2launch.html EC2Launch> in the Amazon Elastic Compute Cloud User Guide.
-- For the @EC2Config@ service, the password is not generated for rebundled AMIs unless @Ec2SetPassword@ is enabled before bundling.
-- The password is encrypted using the key pair that you specified when you launched the instance. You must provide the corresponding key pair file.
-- When you launch an instance, password generation and encryption may take a few minutes. If you try to retrieve the password before it's available, the output returns an empty string. We recommend that you wait up to 15 minutes after launching an instance before trying to retrieve the generated password.
module Network.AWS.EC2.GetPasswordData
    (
    -- * Creating a request
      GetPasswordData (..)
    , mkGetPasswordData
    -- ** Request lenses
    , gpdInstanceId
    , gpdDryRun

    -- * Destructuring the response
    , GetPasswordDataResponse (..)
    , mkGetPasswordDataResponse
    -- ** Response lenses
    , gpdrrsInstanceId
    , gpdrrsPasswordData
    , gpdrrsTimestamp
    , gpdrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetPasswordData' smart constructor.
data GetPasswordData = GetPasswordData'
  { instanceId :: Types.InstanceId
    -- ^ The ID of the Windows instance.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPasswordData' value with any optional fields omitted.
mkGetPasswordData
    :: Types.InstanceId -- ^ 'instanceId'
    -> GetPasswordData
mkGetPasswordData instanceId
  = GetPasswordData'{instanceId, dryRun = Core.Nothing}

-- | The ID of the Windows instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpdInstanceId :: Lens.Lens' GetPasswordData Types.InstanceId
gpdInstanceId = Lens.field @"instanceId"
{-# INLINEABLE gpdInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpdDryRun :: Lens.Lens' GetPasswordData (Core.Maybe Core.Bool)
gpdDryRun = Lens.field @"dryRun"
{-# INLINEABLE gpdDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery GetPasswordData where
        toQuery GetPasswordData{..}
          = Core.toQueryPair "Action" ("GetPasswordData" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "InstanceId" instanceId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders GetPasswordData where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetPasswordData where
        type Rs GetPasswordData = GetPasswordDataResponse
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
                 GetPasswordDataResponse' Core.<$>
                   (x Core..@ "instanceId") Core.<*> x Core..@ "passwordData" Core.<*>
                     x Core..@ "timestamp"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetPasswordDataResponse' smart constructor.
data GetPasswordDataResponse = GetPasswordDataResponse'
  { instanceId :: Core.Text
    -- ^ The ID of the Windows instance.
  , passwordData :: Core.Text
    -- ^ The password of the instance. Returns an empty string if the password is not available.
  , timestamp :: Core.UTCTime
    -- ^ The time the data was last updated.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetPasswordDataResponse' value with any optional fields omitted.
mkGetPasswordDataResponse
    :: Core.Text -- ^ 'instanceId'
    -> Core.Text -- ^ 'passwordData'
    -> Core.UTCTime -- ^ 'timestamp'
    -> Core.Int -- ^ 'responseStatus'
    -> GetPasswordDataResponse
mkGetPasswordDataResponse instanceId passwordData timestamp
  responseStatus
  = GetPasswordDataResponse'{instanceId, passwordData, timestamp,
                             responseStatus}

-- | The ID of the Windows instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpdrrsInstanceId :: Lens.Lens' GetPasswordDataResponse Core.Text
gpdrrsInstanceId = Lens.field @"instanceId"
{-# INLINEABLE gpdrrsInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The password of the instance. Returns an empty string if the password is not available.
--
-- /Note:/ Consider using 'passwordData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpdrrsPasswordData :: Lens.Lens' GetPasswordDataResponse Core.Text
gpdrrsPasswordData = Lens.field @"passwordData"
{-# INLINEABLE gpdrrsPasswordData #-}
{-# DEPRECATED passwordData "Use generic-lens or generic-optics with 'passwordData' instead"  #-}

-- | The time the data was last updated.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpdrrsTimestamp :: Lens.Lens' GetPasswordDataResponse Core.UTCTime
gpdrrsTimestamp = Lens.field @"timestamp"
{-# INLINEABLE gpdrrsTimestamp #-}
{-# DEPRECATED timestamp "Use generic-lens or generic-optics with 'timestamp' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpdrrsResponseStatus :: Lens.Lens' GetPasswordDataResponse Core.Int
gpdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gpdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
