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
    urjFrequency,
    urjNumberOfRecentAMIsToKeep,
    urjLicenseType,
    urjRoleName,
    urjEncrypted,
    urjReplicationJobId,
    urjNextReplicationRunStartTime,
    urjKmsKeyId,
    urjDescription,

    -- * Destructuring the response
    UpdateReplicationJobResponse (..),
    mkUpdateReplicationJobResponse,

    -- ** Response lenses
    urjrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SMS.Types

-- | /See:/ 'mkUpdateReplicationJob' smart constructor.
data UpdateReplicationJob = UpdateReplicationJob'
  { -- | The time between consecutive replication runs, in hours.
    frequency :: Lude.Maybe Lude.Int,
    -- | The maximum number of SMS-created AMIs to retain. The oldest is deleted after the maximum number is reached and a new AMI is created.
    numberOfRecentAMIsToKeep :: Lude.Maybe Lude.Int,
    -- | The license type to be used for the AMI created by a successful replication run.
    licenseType :: Lude.Maybe LicenseType,
    -- | The name of the IAM role to be used by AWS SMS.
    roleName :: Lude.Maybe Lude.Text,
    -- | When true, the replication job produces encrypted AMIs. For more information, @KmsKeyId@ .
    encrypted :: Lude.Maybe Lude.Bool,
    -- | The ID of the replication job.
    replicationJobId :: Lude.Text,
    -- | The start time of the next replication run.
    nextReplicationRunStartTime :: Lude.Maybe Lude.Timestamp,
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
    kmsKeyId :: Lude.Maybe Lude.Text,
    -- | The description of the replication job.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateReplicationJob' with the minimum fields required to make a request.
--
-- * 'frequency' - The time between consecutive replication runs, in hours.
-- * 'numberOfRecentAMIsToKeep' - The maximum number of SMS-created AMIs to retain. The oldest is deleted after the maximum number is reached and a new AMI is created.
-- * 'licenseType' - The license type to be used for the AMI created by a successful replication run.
-- * 'roleName' - The name of the IAM role to be used by AWS SMS.
-- * 'encrypted' - When true, the replication job produces encrypted AMIs. For more information, @KmsKeyId@ .
-- * 'replicationJobId' - The ID of the replication job.
-- * 'nextReplicationRunStartTime' - The start time of the next replication run.
-- * 'kmsKeyId' - The ID of the KMS key for replication jobs that produce encrypted AMIs. This value can be any of the following:
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
-- * 'description' - The description of the replication job.
mkUpdateReplicationJob ::
  -- | 'replicationJobId'
  Lude.Text ->
  UpdateReplicationJob
mkUpdateReplicationJob pReplicationJobId_ =
  UpdateReplicationJob'
    { frequency = Lude.Nothing,
      numberOfRecentAMIsToKeep = Lude.Nothing,
      licenseType = Lude.Nothing,
      roleName = Lude.Nothing,
      encrypted = Lude.Nothing,
      replicationJobId = pReplicationJobId_,
      nextReplicationRunStartTime = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The time between consecutive replication runs, in hours.
--
-- /Note:/ Consider using 'frequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urjFrequency :: Lens.Lens' UpdateReplicationJob (Lude.Maybe Lude.Int)
urjFrequency = Lens.lens (frequency :: UpdateReplicationJob -> Lude.Maybe Lude.Int) (\s a -> s {frequency = a} :: UpdateReplicationJob)
{-# DEPRECATED urjFrequency "Use generic-lens or generic-optics with 'frequency' instead." #-}

-- | The maximum number of SMS-created AMIs to retain. The oldest is deleted after the maximum number is reached and a new AMI is created.
--
-- /Note:/ Consider using 'numberOfRecentAMIsToKeep' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urjNumberOfRecentAMIsToKeep :: Lens.Lens' UpdateReplicationJob (Lude.Maybe Lude.Int)
urjNumberOfRecentAMIsToKeep = Lens.lens (numberOfRecentAMIsToKeep :: UpdateReplicationJob -> Lude.Maybe Lude.Int) (\s a -> s {numberOfRecentAMIsToKeep = a} :: UpdateReplicationJob)
{-# DEPRECATED urjNumberOfRecentAMIsToKeep "Use generic-lens or generic-optics with 'numberOfRecentAMIsToKeep' instead." #-}

-- | The license type to be used for the AMI created by a successful replication run.
--
-- /Note:/ Consider using 'licenseType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urjLicenseType :: Lens.Lens' UpdateReplicationJob (Lude.Maybe LicenseType)
urjLicenseType = Lens.lens (licenseType :: UpdateReplicationJob -> Lude.Maybe LicenseType) (\s a -> s {licenseType = a} :: UpdateReplicationJob)
{-# DEPRECATED urjLicenseType "Use generic-lens or generic-optics with 'licenseType' instead." #-}

-- | The name of the IAM role to be used by AWS SMS.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urjRoleName :: Lens.Lens' UpdateReplicationJob (Lude.Maybe Lude.Text)
urjRoleName = Lens.lens (roleName :: UpdateReplicationJob -> Lude.Maybe Lude.Text) (\s a -> s {roleName = a} :: UpdateReplicationJob)
{-# DEPRECATED urjRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | When true, the replication job produces encrypted AMIs. For more information, @KmsKeyId@ .
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urjEncrypted :: Lens.Lens' UpdateReplicationJob (Lude.Maybe Lude.Bool)
urjEncrypted = Lens.lens (encrypted :: UpdateReplicationJob -> Lude.Maybe Lude.Bool) (\s a -> s {encrypted = a} :: UpdateReplicationJob)
{-# DEPRECATED urjEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | The ID of the replication job.
--
-- /Note:/ Consider using 'replicationJobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urjReplicationJobId :: Lens.Lens' UpdateReplicationJob Lude.Text
urjReplicationJobId = Lens.lens (replicationJobId :: UpdateReplicationJob -> Lude.Text) (\s a -> s {replicationJobId = a} :: UpdateReplicationJob)
{-# DEPRECATED urjReplicationJobId "Use generic-lens or generic-optics with 'replicationJobId' instead." #-}

-- | The start time of the next replication run.
--
-- /Note:/ Consider using 'nextReplicationRunStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urjNextReplicationRunStartTime :: Lens.Lens' UpdateReplicationJob (Lude.Maybe Lude.Timestamp)
urjNextReplicationRunStartTime = Lens.lens (nextReplicationRunStartTime :: UpdateReplicationJob -> Lude.Maybe Lude.Timestamp) (\s a -> s {nextReplicationRunStartTime = a} :: UpdateReplicationJob)
{-# DEPRECATED urjNextReplicationRunStartTime "Use generic-lens or generic-optics with 'nextReplicationRunStartTime' instead." #-}

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
urjKmsKeyId :: Lens.Lens' UpdateReplicationJob (Lude.Maybe Lude.Text)
urjKmsKeyId = Lens.lens (kmsKeyId :: UpdateReplicationJob -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: UpdateReplicationJob)
{-# DEPRECATED urjKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The description of the replication job.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urjDescription :: Lens.Lens' UpdateReplicationJob (Lude.Maybe Lude.Text)
urjDescription = Lens.lens (description :: UpdateReplicationJob -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateReplicationJob)
{-# DEPRECATED urjDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest UpdateReplicationJob where
  type Rs UpdateReplicationJob = UpdateReplicationJobResponse
  request = Req.postJSON smsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateReplicationJobResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateReplicationJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSServerMigrationService_V2016_10_24.UpdateReplicationJob" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateReplicationJob where
  toJSON UpdateReplicationJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("frequency" Lude..=) Lude.<$> frequency,
            ("numberOfRecentAmisToKeep" Lude..=)
              Lude.<$> numberOfRecentAMIsToKeep,
            ("licenseType" Lude..=) Lude.<$> licenseType,
            ("roleName" Lude..=) Lude.<$> roleName,
            ("encrypted" Lude..=) Lude.<$> encrypted,
            Lude.Just ("replicationJobId" Lude..= replicationJobId),
            ("nextReplicationRunStartTime" Lude..=)
              Lude.<$> nextReplicationRunStartTime,
            ("kmsKeyId" Lude..=) Lude.<$> kmsKeyId,
            ("description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath UpdateReplicationJob where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateReplicationJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateReplicationJobResponse' smart constructor.
newtype UpdateReplicationJobResponse = UpdateReplicationJobResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateReplicationJobResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateReplicationJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateReplicationJobResponse
mkUpdateReplicationJobResponse pResponseStatus_ =
  UpdateReplicationJobResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urjrsResponseStatus :: Lens.Lens' UpdateReplicationJobResponse Lude.Int
urjrsResponseStatus = Lens.lens (responseStatus :: UpdateReplicationJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateReplicationJobResponse)
{-# DEPRECATED urjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
