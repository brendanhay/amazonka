{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.CreateReplicationJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a replication job. The replication job schedules periodic replication runs to replicate your server to AWS. Each replication run creates an Amazon Machine Image (AMI).
module Network.AWS.SMS.CreateReplicationJob
    (
    -- * Creating a request
      CreateReplicationJob (..)
    , mkCreateReplicationJob
    -- ** Request lenses
    , crjServerId
    , crjSeedReplicationTime
    , crjDescription
    , crjEncrypted
    , crjFrequency
    , crjKmsKeyId
    , crjLicenseType
    , crjNumberOfRecentAmisToKeep
    , crjRoleName
    , crjRunOnce

    -- * Destructuring the response
    , CreateReplicationJobResponse (..)
    , mkCreateReplicationJobResponse
    -- ** Response lenses
    , crjrrsReplicationJobId
    , crjrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkCreateReplicationJob' smart constructor.
data CreateReplicationJob = CreateReplicationJob'
  { serverId :: Types.ServerId
    -- ^ The ID of the server.
  , seedReplicationTime :: Core.NominalDiffTime
    -- ^ The seed replication time.
  , description :: Core.Maybe Types.Description
    -- ^ The description of the replication job.
  , encrypted :: Core.Maybe Core.Bool
    -- ^ Indicates whether the replication job produces encrypted AMIs.
  , frequency :: Core.Maybe Core.Int
    -- ^ The time between consecutive replication runs, in hours.
  , kmsKeyId :: Core.Maybe Types.KmsKeyId
    -- ^ The ID of the KMS key for replication jobs that produce encrypted AMIs. This value can be any of the following:
--
--
--     * KMS key ID
--
--
--     * KMS key alias
--
--
--     * ARN referring to the KMS key ID
--
--
--     * ARN referring to the KMS key alias
--
--
-- If encrypted is /true/ but a KMS key ID is not specified, the customer's default KMS key for Amazon EBS is used. 
  , licenseType :: Core.Maybe Types.LicenseType
    -- ^ The license type to be used for the AMI created by a successful replication run.
  , numberOfRecentAmisToKeep :: Core.Maybe Core.Int
    -- ^ The maximum number of SMS-created AMIs to retain. The oldest is deleted after the maximum number is reached and a new AMI is created.
  , roleName :: Core.Maybe Types.RoleName
    -- ^ The name of the IAM role to be used by the AWS SMS.
  , runOnce :: Core.Maybe Core.Bool
    -- ^ Indicates whether to run the replication job one time.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateReplicationJob' value with any optional fields omitted.
mkCreateReplicationJob
    :: Types.ServerId -- ^ 'serverId'
    -> Core.NominalDiffTime -- ^ 'seedReplicationTime'
    -> CreateReplicationJob
mkCreateReplicationJob serverId seedReplicationTime
  = CreateReplicationJob'{serverId, seedReplicationTime,
                          description = Core.Nothing, encrypted = Core.Nothing,
                          frequency = Core.Nothing, kmsKeyId = Core.Nothing,
                          licenseType = Core.Nothing,
                          numberOfRecentAmisToKeep = Core.Nothing, roleName = Core.Nothing,
                          runOnce = Core.Nothing}

-- | The ID of the server.
--
-- /Note:/ Consider using 'serverId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crjServerId :: Lens.Lens' CreateReplicationJob Types.ServerId
crjServerId = Lens.field @"serverId"
{-# INLINEABLE crjServerId #-}
{-# DEPRECATED serverId "Use generic-lens or generic-optics with 'serverId' instead"  #-}

-- | The seed replication time.
--
-- /Note:/ Consider using 'seedReplicationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crjSeedReplicationTime :: Lens.Lens' CreateReplicationJob Core.NominalDiffTime
crjSeedReplicationTime = Lens.field @"seedReplicationTime"
{-# INLINEABLE crjSeedReplicationTime #-}
{-# DEPRECATED seedReplicationTime "Use generic-lens or generic-optics with 'seedReplicationTime' instead"  #-}

-- | The description of the replication job.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crjDescription :: Lens.Lens' CreateReplicationJob (Core.Maybe Types.Description)
crjDescription = Lens.field @"description"
{-# INLINEABLE crjDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Indicates whether the replication job produces encrypted AMIs.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crjEncrypted :: Lens.Lens' CreateReplicationJob (Core.Maybe Core.Bool)
crjEncrypted = Lens.field @"encrypted"
{-# INLINEABLE crjEncrypted #-}
{-# DEPRECATED encrypted "Use generic-lens or generic-optics with 'encrypted' instead"  #-}

-- | The time between consecutive replication runs, in hours.
--
-- /Note:/ Consider using 'frequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crjFrequency :: Lens.Lens' CreateReplicationJob (Core.Maybe Core.Int)
crjFrequency = Lens.field @"frequency"
{-# INLINEABLE crjFrequency #-}
{-# DEPRECATED frequency "Use generic-lens or generic-optics with 'frequency' instead"  #-}

-- | The ID of the KMS key for replication jobs that produce encrypted AMIs. This value can be any of the following:
--
--
--     * KMS key ID
--
--
--     * KMS key alias
--
--
--     * ARN referring to the KMS key ID
--
--
--     * ARN referring to the KMS key alias
--
--
-- If encrypted is /true/ but a KMS key ID is not specified, the customer's default KMS key for Amazon EBS is used. 
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crjKmsKeyId :: Lens.Lens' CreateReplicationJob (Core.Maybe Types.KmsKeyId)
crjKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE crjKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | The license type to be used for the AMI created by a successful replication run.
--
-- /Note:/ Consider using 'licenseType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crjLicenseType :: Lens.Lens' CreateReplicationJob (Core.Maybe Types.LicenseType)
crjLicenseType = Lens.field @"licenseType"
{-# INLINEABLE crjLicenseType #-}
{-# DEPRECATED licenseType "Use generic-lens or generic-optics with 'licenseType' instead"  #-}

-- | The maximum number of SMS-created AMIs to retain. The oldest is deleted after the maximum number is reached and a new AMI is created.
--
-- /Note:/ Consider using 'numberOfRecentAmisToKeep' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crjNumberOfRecentAmisToKeep :: Lens.Lens' CreateReplicationJob (Core.Maybe Core.Int)
crjNumberOfRecentAmisToKeep = Lens.field @"numberOfRecentAmisToKeep"
{-# INLINEABLE crjNumberOfRecentAmisToKeep #-}
{-# DEPRECATED numberOfRecentAmisToKeep "Use generic-lens or generic-optics with 'numberOfRecentAmisToKeep' instead"  #-}

-- | The name of the IAM role to be used by the AWS SMS.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crjRoleName :: Lens.Lens' CreateReplicationJob (Core.Maybe Types.RoleName)
crjRoleName = Lens.field @"roleName"
{-# INLINEABLE crjRoleName #-}
{-# DEPRECATED roleName "Use generic-lens or generic-optics with 'roleName' instead"  #-}

-- | Indicates whether to run the replication job one time.
--
-- /Note:/ Consider using 'runOnce' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crjRunOnce :: Lens.Lens' CreateReplicationJob (Core.Maybe Core.Bool)
crjRunOnce = Lens.field @"runOnce"
{-# INLINEABLE crjRunOnce #-}
{-# DEPRECATED runOnce "Use generic-lens or generic-optics with 'runOnce' instead"  #-}

instance Core.ToQuery CreateReplicationJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateReplicationJob where
        toHeaders CreateReplicationJob{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSServerMigrationService_V2016_10_24.CreateReplicationJob")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateReplicationJob where
        toJSON CreateReplicationJob{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("serverId" Core..= serverId),
                  Core.Just ("seedReplicationTime" Core..= seedReplicationTime),
                  ("description" Core..=) Core.<$> description,
                  ("encrypted" Core..=) Core.<$> encrypted,
                  ("frequency" Core..=) Core.<$> frequency,
                  ("kmsKeyId" Core..=) Core.<$> kmsKeyId,
                  ("licenseType" Core..=) Core.<$> licenseType,
                  ("numberOfRecentAmisToKeep" Core..=) Core.<$>
                    numberOfRecentAmisToKeep,
                  ("roleName" Core..=) Core.<$> roleName,
                  ("runOnce" Core..=) Core.<$> runOnce])

instance Core.AWSRequest CreateReplicationJob where
        type Rs CreateReplicationJob = CreateReplicationJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateReplicationJobResponse' Core.<$>
                   (x Core..:? "replicationJobId") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateReplicationJobResponse' smart constructor.
data CreateReplicationJobResponse = CreateReplicationJobResponse'
  { replicationJobId :: Core.Maybe Types.ReplicationJobId
    -- ^ The unique identifier of the replication job.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateReplicationJobResponse' value with any optional fields omitted.
mkCreateReplicationJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateReplicationJobResponse
mkCreateReplicationJobResponse responseStatus
  = CreateReplicationJobResponse'{replicationJobId = Core.Nothing,
                                  responseStatus}

-- | The unique identifier of the replication job.
--
-- /Note:/ Consider using 'replicationJobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crjrrsReplicationJobId :: Lens.Lens' CreateReplicationJobResponse (Core.Maybe Types.ReplicationJobId)
crjrrsReplicationJobId = Lens.field @"replicationJobId"
{-# INLINEABLE crjrrsReplicationJobId #-}
{-# DEPRECATED replicationJobId "Use generic-lens or generic-optics with 'replicationJobId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crjrrsResponseStatus :: Lens.Lens' CreateReplicationJobResponse Core.Int
crjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
