{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.UpdateReplicationJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified settings for the specified replication job.
module Network.AWS.SMS.UpdateReplicationJob
  ( -- * Creating a request
    UpdateReplicationJob (..),
    mkUpdateReplicationJob,

    -- ** Request lenses
    urjReplicationJobId,
    urjDescription,
    urjEncrypted,
    urjFrequency,
    urjKmsKeyId,
    urjLicenseType,
    urjNextReplicationRunStartTime,
    urjNumberOfRecentAmisToKeep,
    urjRoleName,

    -- * Destructuring the response
    UpdateReplicationJobResponse (..),
    mkUpdateReplicationJobResponse,

    -- ** Response lenses
    urjrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkUpdateReplicationJob' smart constructor.
data UpdateReplicationJob = UpdateReplicationJob'
  { -- | The ID of the replication job.
    replicationJobId :: Types.ReplicationJobId,
    -- | The description of the replication job.
    description :: Core.Maybe Types.Description,
    -- | When true, the replication job produces encrypted AMIs. For more information, @KmsKeyId@ .
    encrypted :: Core.Maybe Core.Bool,
    -- | The time between consecutive replication runs, in hours.
    frequency :: Core.Maybe Core.Int,
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
    -- If encrypted is enabled but a KMS key ID is not specified, the customer's default KMS key for Amazon EBS is used.
    kmsKeyId :: Core.Maybe Types.KmsKeyId,
    -- | The license type to be used for the AMI created by a successful replication run.
    licenseType :: Core.Maybe Types.LicenseType,
    -- | The start time of the next replication run.
    nextReplicationRunStartTime :: Core.Maybe Core.NominalDiffTime,
    -- | The maximum number of SMS-created AMIs to retain. The oldest is deleted after the maximum number is reached and a new AMI is created.
    numberOfRecentAmisToKeep :: Core.Maybe Core.Int,
    -- | The name of the IAM role to be used by AWS SMS.
    roleName :: Core.Maybe Types.RoleName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateReplicationJob' value with any optional fields omitted.
mkUpdateReplicationJob ::
  -- | 'replicationJobId'
  Types.ReplicationJobId ->
  UpdateReplicationJob
mkUpdateReplicationJob replicationJobId =
  UpdateReplicationJob'
    { replicationJobId,
      description = Core.Nothing,
      encrypted = Core.Nothing,
      frequency = Core.Nothing,
      kmsKeyId = Core.Nothing,
      licenseType = Core.Nothing,
      nextReplicationRunStartTime = Core.Nothing,
      numberOfRecentAmisToKeep = Core.Nothing,
      roleName = Core.Nothing
    }

-- | The ID of the replication job.
--
-- /Note:/ Consider using 'replicationJobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urjReplicationJobId :: Lens.Lens' UpdateReplicationJob Types.ReplicationJobId
urjReplicationJobId = Lens.field @"replicationJobId"
{-# DEPRECATED urjReplicationJobId "Use generic-lens or generic-optics with 'replicationJobId' instead." #-}

-- | The description of the replication job.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urjDescription :: Lens.Lens' UpdateReplicationJob (Core.Maybe Types.Description)
urjDescription = Lens.field @"description"
{-# DEPRECATED urjDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | When true, the replication job produces encrypted AMIs. For more information, @KmsKeyId@ .
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urjEncrypted :: Lens.Lens' UpdateReplicationJob (Core.Maybe Core.Bool)
urjEncrypted = Lens.field @"encrypted"
{-# DEPRECATED urjEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | The time between consecutive replication runs, in hours.
--
-- /Note:/ Consider using 'frequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urjFrequency :: Lens.Lens' UpdateReplicationJob (Core.Maybe Core.Int)
urjFrequency = Lens.field @"frequency"
{-# DEPRECATED urjFrequency "Use generic-lens or generic-optics with 'frequency' instead." #-}

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
-- If encrypted is enabled but a KMS key ID is not specified, the customer's default KMS key for Amazon EBS is used.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urjKmsKeyId :: Lens.Lens' UpdateReplicationJob (Core.Maybe Types.KmsKeyId)
urjKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED urjKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The license type to be used for the AMI created by a successful replication run.
--
-- /Note:/ Consider using 'licenseType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urjLicenseType :: Lens.Lens' UpdateReplicationJob (Core.Maybe Types.LicenseType)
urjLicenseType = Lens.field @"licenseType"
{-# DEPRECATED urjLicenseType "Use generic-lens or generic-optics with 'licenseType' instead." #-}

-- | The start time of the next replication run.
--
-- /Note:/ Consider using 'nextReplicationRunStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urjNextReplicationRunStartTime :: Lens.Lens' UpdateReplicationJob (Core.Maybe Core.NominalDiffTime)
urjNextReplicationRunStartTime = Lens.field @"nextReplicationRunStartTime"
{-# DEPRECATED urjNextReplicationRunStartTime "Use generic-lens or generic-optics with 'nextReplicationRunStartTime' instead." #-}

-- | The maximum number of SMS-created AMIs to retain. The oldest is deleted after the maximum number is reached and a new AMI is created.
--
-- /Note:/ Consider using 'numberOfRecentAmisToKeep' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urjNumberOfRecentAmisToKeep :: Lens.Lens' UpdateReplicationJob (Core.Maybe Core.Int)
urjNumberOfRecentAmisToKeep = Lens.field @"numberOfRecentAmisToKeep"
{-# DEPRECATED urjNumberOfRecentAmisToKeep "Use generic-lens or generic-optics with 'numberOfRecentAmisToKeep' instead." #-}

-- | The name of the IAM role to be used by AWS SMS.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urjRoleName :: Lens.Lens' UpdateReplicationJob (Core.Maybe Types.RoleName)
urjRoleName = Lens.field @"roleName"
{-# DEPRECATED urjRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

instance Core.FromJSON UpdateReplicationJob where
  toJSON UpdateReplicationJob {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("replicationJobId" Core..= replicationJobId),
            ("description" Core..=) Core.<$> description,
            ("encrypted" Core..=) Core.<$> encrypted,
            ("frequency" Core..=) Core.<$> frequency,
            ("kmsKeyId" Core..=) Core.<$> kmsKeyId,
            ("licenseType" Core..=) Core.<$> licenseType,
            ("nextReplicationRunStartTime" Core..=)
              Core.<$> nextReplicationRunStartTime,
            ("numberOfRecentAmisToKeep" Core..=)
              Core.<$> numberOfRecentAmisToKeep,
            ("roleName" Core..=) Core.<$> roleName
          ]
      )

instance Core.AWSRequest UpdateReplicationJob where
  type Rs UpdateReplicationJob = UpdateReplicationJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSServerMigrationService_V2016_10_24.UpdateReplicationJob"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateReplicationJobResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateReplicationJobResponse' smart constructor.
newtype UpdateReplicationJobResponse = UpdateReplicationJobResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateReplicationJobResponse' value with any optional fields omitted.
mkUpdateReplicationJobResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateReplicationJobResponse
mkUpdateReplicationJobResponse responseStatus =
  UpdateReplicationJobResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urjrrsResponseStatus :: Lens.Lens' UpdateReplicationJobResponse Core.Int
urjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED urjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
