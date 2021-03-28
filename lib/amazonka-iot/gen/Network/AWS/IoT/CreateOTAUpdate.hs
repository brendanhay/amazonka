{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateOTAUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS IoT OTAUpdate on a target group of things or groups.
module Network.AWS.IoT.CreateOTAUpdate
    (
    -- * Creating a request
      CreateOTAUpdate (..)
    , mkCreateOTAUpdate
    -- ** Request lenses
    , cotauOtaUpdateId
    , cotauTargets
    , cotauFiles
    , cotauRoleArn
    , cotauAdditionalParameters
    , cotauAwsJobAbortConfig
    , cotauAwsJobExecutionsRolloutConfig
    , cotauAwsJobPresignedUrlConfig
    , cotauAwsJobTimeoutConfig
    , cotauDescription
    , cotauProtocols
    , cotauTags
    , cotauTargetSelection

    -- * Destructuring the response
    , CreateOTAUpdateResponse (..)
    , mkCreateOTAUpdateResponse
    -- ** Response lenses
    , cotaurrsAwsIotJobArn
    , cotaurrsAwsIotJobId
    , cotaurrsOtaUpdateArn
    , cotaurrsOtaUpdateId
    , cotaurrsOtaUpdateStatus
    , cotaurrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateOTAUpdate' smart constructor.
data CreateOTAUpdate = CreateOTAUpdate'
  { otaUpdateId :: Types.OtaUpdateId
    -- ^ The ID of the OTA update to be created.
  , targets :: Core.NonEmpty Types.Target
    -- ^ The devices targeted to receive OTA updates.
  , files :: Core.NonEmpty Types.OTAUpdateFile
    -- ^ The files to be streamed by the OTA update.
  , roleArn :: Types.RoleArn
    -- ^ The IAM role that grants AWS IoT access to the Amazon S3, AWS IoT jobs and AWS Code Signing resources to create an OTA update job.
  , additionalParameters :: Core.Maybe (Core.HashMap Types.AttributeKey Types.Value)
    -- ^ A list of additional OTA update parameters which are name-value pairs.
  , awsJobAbortConfig :: Core.Maybe Types.AwsJobAbortConfig
    -- ^ The criteria that determine when and how a job abort takes place.
  , awsJobExecutionsRolloutConfig :: Core.Maybe Types.AwsJobExecutionsRolloutConfig
    -- ^ Configuration for the rollout of OTA updates.
  , awsJobPresignedUrlConfig :: Core.Maybe Types.AwsJobPresignedUrlConfig
    -- ^ Configuration information for pre-signed URLs.
  , awsJobTimeoutConfig :: Core.Maybe Types.AwsJobTimeoutConfig
    -- ^ Specifies the amount of time each device has to finish its execution of the job. A timer is started when the job execution status is set to @IN_PROGRESS@ . If the job execution status is not set to another terminal state before the timer expires, it will be automatically set to @TIMED_OUT@ .
  , description :: Core.Maybe Types.OTAUpdateDescription
    -- ^ The description of the OTA update.
  , protocols :: Core.Maybe (Core.NonEmpty Types.Protocol)
    -- ^ The protocol used to transfer the OTA update image. Valid values are [HTTP], [MQTT], [HTTP, MQTT]. When both HTTP and MQTT are specified, the target device can choose the protocol.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Metadata which can be used to manage updates.
  , targetSelection :: Core.Maybe Types.TargetSelection
    -- ^ Specifies whether the update will continue to run (CONTINUOUS), or will be complete after all the things specified as targets have completed the update (SNAPSHOT). If continuous, the update may also be run on a thing when a change is detected in a target. For example, an update will run on a thing when the thing is added to a target group, even after the update was completed by all things originally in the group. Valid values: CONTINUOUS | SNAPSHOT.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateOTAUpdate' value with any optional fields omitted.
mkCreateOTAUpdate
    :: Types.OtaUpdateId -- ^ 'otaUpdateId'
    -> Core.NonEmpty Types.Target -- ^ 'targets'
    -> Core.NonEmpty Types.OTAUpdateFile -- ^ 'files'
    -> Types.RoleArn -- ^ 'roleArn'
    -> CreateOTAUpdate
mkCreateOTAUpdate otaUpdateId targets files roleArn
  = CreateOTAUpdate'{otaUpdateId, targets, files, roleArn,
                     additionalParameters = Core.Nothing,
                     awsJobAbortConfig = Core.Nothing,
                     awsJobExecutionsRolloutConfig = Core.Nothing,
                     awsJobPresignedUrlConfig = Core.Nothing,
                     awsJobTimeoutConfig = Core.Nothing, description = Core.Nothing,
                     protocols = Core.Nothing, tags = Core.Nothing,
                     targetSelection = Core.Nothing}

-- | The ID of the OTA update to be created.
--
-- /Note:/ Consider using 'otaUpdateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cotauOtaUpdateId :: Lens.Lens' CreateOTAUpdate Types.OtaUpdateId
cotauOtaUpdateId = Lens.field @"otaUpdateId"
{-# INLINEABLE cotauOtaUpdateId #-}
{-# DEPRECATED otaUpdateId "Use generic-lens or generic-optics with 'otaUpdateId' instead"  #-}

-- | The devices targeted to receive OTA updates.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cotauTargets :: Lens.Lens' CreateOTAUpdate (Core.NonEmpty Types.Target)
cotauTargets = Lens.field @"targets"
{-# INLINEABLE cotauTargets #-}
{-# DEPRECATED targets "Use generic-lens or generic-optics with 'targets' instead"  #-}

-- | The files to be streamed by the OTA update.
--
-- /Note:/ Consider using 'files' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cotauFiles :: Lens.Lens' CreateOTAUpdate (Core.NonEmpty Types.OTAUpdateFile)
cotauFiles = Lens.field @"files"
{-# INLINEABLE cotauFiles #-}
{-# DEPRECATED files "Use generic-lens or generic-optics with 'files' instead"  #-}

-- | The IAM role that grants AWS IoT access to the Amazon S3, AWS IoT jobs and AWS Code Signing resources to create an OTA update job.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cotauRoleArn :: Lens.Lens' CreateOTAUpdate Types.RoleArn
cotauRoleArn = Lens.field @"roleArn"
{-# INLINEABLE cotauRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | A list of additional OTA update parameters which are name-value pairs.
--
-- /Note:/ Consider using 'additionalParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cotauAdditionalParameters :: Lens.Lens' CreateOTAUpdate (Core.Maybe (Core.HashMap Types.AttributeKey Types.Value))
cotauAdditionalParameters = Lens.field @"additionalParameters"
{-# INLINEABLE cotauAdditionalParameters #-}
{-# DEPRECATED additionalParameters "Use generic-lens or generic-optics with 'additionalParameters' instead"  #-}

-- | The criteria that determine when and how a job abort takes place.
--
-- /Note:/ Consider using 'awsJobAbortConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cotauAwsJobAbortConfig :: Lens.Lens' CreateOTAUpdate (Core.Maybe Types.AwsJobAbortConfig)
cotauAwsJobAbortConfig = Lens.field @"awsJobAbortConfig"
{-# INLINEABLE cotauAwsJobAbortConfig #-}
{-# DEPRECATED awsJobAbortConfig "Use generic-lens or generic-optics with 'awsJobAbortConfig' instead"  #-}

-- | Configuration for the rollout of OTA updates.
--
-- /Note:/ Consider using 'awsJobExecutionsRolloutConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cotauAwsJobExecutionsRolloutConfig :: Lens.Lens' CreateOTAUpdate (Core.Maybe Types.AwsJobExecutionsRolloutConfig)
cotauAwsJobExecutionsRolloutConfig = Lens.field @"awsJobExecutionsRolloutConfig"
{-# INLINEABLE cotauAwsJobExecutionsRolloutConfig #-}
{-# DEPRECATED awsJobExecutionsRolloutConfig "Use generic-lens or generic-optics with 'awsJobExecutionsRolloutConfig' instead"  #-}

-- | Configuration information for pre-signed URLs.
--
-- /Note:/ Consider using 'awsJobPresignedUrlConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cotauAwsJobPresignedUrlConfig :: Lens.Lens' CreateOTAUpdate (Core.Maybe Types.AwsJobPresignedUrlConfig)
cotauAwsJobPresignedUrlConfig = Lens.field @"awsJobPresignedUrlConfig"
{-# INLINEABLE cotauAwsJobPresignedUrlConfig #-}
{-# DEPRECATED awsJobPresignedUrlConfig "Use generic-lens or generic-optics with 'awsJobPresignedUrlConfig' instead"  #-}

-- | Specifies the amount of time each device has to finish its execution of the job. A timer is started when the job execution status is set to @IN_PROGRESS@ . If the job execution status is not set to another terminal state before the timer expires, it will be automatically set to @TIMED_OUT@ .
--
-- /Note:/ Consider using 'awsJobTimeoutConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cotauAwsJobTimeoutConfig :: Lens.Lens' CreateOTAUpdate (Core.Maybe Types.AwsJobTimeoutConfig)
cotauAwsJobTimeoutConfig = Lens.field @"awsJobTimeoutConfig"
{-# INLINEABLE cotauAwsJobTimeoutConfig #-}
{-# DEPRECATED awsJobTimeoutConfig "Use generic-lens or generic-optics with 'awsJobTimeoutConfig' instead"  #-}

-- | The description of the OTA update.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cotauDescription :: Lens.Lens' CreateOTAUpdate (Core.Maybe Types.OTAUpdateDescription)
cotauDescription = Lens.field @"description"
{-# INLINEABLE cotauDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The protocol used to transfer the OTA update image. Valid values are [HTTP], [MQTT], [HTTP, MQTT]. When both HTTP and MQTT are specified, the target device can choose the protocol.
--
-- /Note:/ Consider using 'protocols' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cotauProtocols :: Lens.Lens' CreateOTAUpdate (Core.Maybe (Core.NonEmpty Types.Protocol))
cotauProtocols = Lens.field @"protocols"
{-# INLINEABLE cotauProtocols #-}
{-# DEPRECATED protocols "Use generic-lens or generic-optics with 'protocols' instead"  #-}

-- | Metadata which can be used to manage updates.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cotauTags :: Lens.Lens' CreateOTAUpdate (Core.Maybe [Types.Tag])
cotauTags = Lens.field @"tags"
{-# INLINEABLE cotauTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | Specifies whether the update will continue to run (CONTINUOUS), or will be complete after all the things specified as targets have completed the update (SNAPSHOT). If continuous, the update may also be run on a thing when a change is detected in a target. For example, an update will run on a thing when the thing is added to a target group, even after the update was completed by all things originally in the group. Valid values: CONTINUOUS | SNAPSHOT.
--
-- /Note:/ Consider using 'targetSelection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cotauTargetSelection :: Lens.Lens' CreateOTAUpdate (Core.Maybe Types.TargetSelection)
cotauTargetSelection = Lens.field @"targetSelection"
{-# INLINEABLE cotauTargetSelection #-}
{-# DEPRECATED targetSelection "Use generic-lens or generic-optics with 'targetSelection' instead"  #-}

instance Core.ToQuery CreateOTAUpdate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateOTAUpdate where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON CreateOTAUpdate where
        toJSON CreateOTAUpdate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("targets" Core..= targets),
                  Core.Just ("files" Core..= files),
                  Core.Just ("roleArn" Core..= roleArn),
                  ("additionalParameters" Core..=) Core.<$> additionalParameters,
                  ("awsJobAbortConfig" Core..=) Core.<$> awsJobAbortConfig,
                  ("awsJobExecutionsRolloutConfig" Core..=) Core.<$>
                    awsJobExecutionsRolloutConfig,
                  ("awsJobPresignedUrlConfig" Core..=) Core.<$>
                    awsJobPresignedUrlConfig,
                  ("awsJobTimeoutConfig" Core..=) Core.<$> awsJobTimeoutConfig,
                  ("description" Core..=) Core.<$> description,
                  ("protocols" Core..=) Core.<$> protocols,
                  ("tags" Core..=) Core.<$> tags,
                  ("targetSelection" Core..=) Core.<$> targetSelection])

instance Core.AWSRequest CreateOTAUpdate where
        type Rs CreateOTAUpdate = CreateOTAUpdateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/otaUpdates/" Core.<> Core.toText otaUpdateId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateOTAUpdateResponse' Core.<$>
                   (x Core..:? "awsIotJobArn") Core.<*> x Core..:? "awsIotJobId"
                     Core.<*> x Core..:? "otaUpdateArn"
                     Core.<*> x Core..:? "otaUpdateId"
                     Core.<*> x Core..:? "otaUpdateStatus"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateOTAUpdateResponse' smart constructor.
data CreateOTAUpdateResponse = CreateOTAUpdateResponse'
  { awsIotJobArn :: Core.Maybe Types.AwsIotJobArn
    -- ^ The AWS IoT job ARN associated with the OTA update.
  , awsIotJobId :: Core.Maybe Types.AwsIotJobId
    -- ^ The AWS IoT job ID associated with the OTA update.
  , otaUpdateArn :: Core.Maybe Types.OtaUpdateArn
    -- ^ The OTA update ARN.
  , otaUpdateId :: Core.Maybe Types.OtaUpdateId
    -- ^ The OTA update ID.
  , otaUpdateStatus :: Core.Maybe Types.OTAUpdateStatus
    -- ^ The OTA update status.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateOTAUpdateResponse' value with any optional fields omitted.
mkCreateOTAUpdateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateOTAUpdateResponse
mkCreateOTAUpdateResponse responseStatus
  = CreateOTAUpdateResponse'{awsIotJobArn = Core.Nothing,
                             awsIotJobId = Core.Nothing, otaUpdateArn = Core.Nothing,
                             otaUpdateId = Core.Nothing, otaUpdateStatus = Core.Nothing,
                             responseStatus}

-- | The AWS IoT job ARN associated with the OTA update.
--
-- /Note:/ Consider using 'awsIotJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cotaurrsAwsIotJobArn :: Lens.Lens' CreateOTAUpdateResponse (Core.Maybe Types.AwsIotJobArn)
cotaurrsAwsIotJobArn = Lens.field @"awsIotJobArn"
{-# INLINEABLE cotaurrsAwsIotJobArn #-}
{-# DEPRECATED awsIotJobArn "Use generic-lens or generic-optics with 'awsIotJobArn' instead"  #-}

-- | The AWS IoT job ID associated with the OTA update.
--
-- /Note:/ Consider using 'awsIotJobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cotaurrsAwsIotJobId :: Lens.Lens' CreateOTAUpdateResponse (Core.Maybe Types.AwsIotJobId)
cotaurrsAwsIotJobId = Lens.field @"awsIotJobId"
{-# INLINEABLE cotaurrsAwsIotJobId #-}
{-# DEPRECATED awsIotJobId "Use generic-lens or generic-optics with 'awsIotJobId' instead"  #-}

-- | The OTA update ARN.
--
-- /Note:/ Consider using 'otaUpdateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cotaurrsOtaUpdateArn :: Lens.Lens' CreateOTAUpdateResponse (Core.Maybe Types.OtaUpdateArn)
cotaurrsOtaUpdateArn = Lens.field @"otaUpdateArn"
{-# INLINEABLE cotaurrsOtaUpdateArn #-}
{-# DEPRECATED otaUpdateArn "Use generic-lens or generic-optics with 'otaUpdateArn' instead"  #-}

-- | The OTA update ID.
--
-- /Note:/ Consider using 'otaUpdateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cotaurrsOtaUpdateId :: Lens.Lens' CreateOTAUpdateResponse (Core.Maybe Types.OtaUpdateId)
cotaurrsOtaUpdateId = Lens.field @"otaUpdateId"
{-# INLINEABLE cotaurrsOtaUpdateId #-}
{-# DEPRECATED otaUpdateId "Use generic-lens or generic-optics with 'otaUpdateId' instead"  #-}

-- | The OTA update status.
--
-- /Note:/ Consider using 'otaUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cotaurrsOtaUpdateStatus :: Lens.Lens' CreateOTAUpdateResponse (Core.Maybe Types.OTAUpdateStatus)
cotaurrsOtaUpdateStatus = Lens.field @"otaUpdateStatus"
{-# INLINEABLE cotaurrsOtaUpdateStatus #-}
{-# DEPRECATED otaUpdateStatus "Use generic-lens or generic-optics with 'otaUpdateStatus' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cotaurrsResponseStatus :: Lens.Lens' CreateOTAUpdateResponse Core.Int
cotaurrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cotaurrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
