{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateReplicationJob (..),
    mkCreateReplicationJob,

    -- ** Request lenses
    crjFrequency,
    crjNumberOfRecentAMIsToKeep,
    crjLicenseType,
    crjRoleName,
    crjEncrypted,
    crjKmsKeyId,
    crjRunOnce,
    crjDescription,
    crjServerId,
    crjSeedReplicationTime,

    -- * Destructuring the response
    CreateReplicationJobResponse (..),
    mkCreateReplicationJobResponse,

    -- ** Response lenses
    crjrsReplicationJobId,
    crjrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SMS.Types

-- | /See:/ 'mkCreateReplicationJob' smart constructor.
data CreateReplicationJob = CreateReplicationJob'
  { frequency ::
      Lude.Maybe Lude.Int,
    numberOfRecentAMIsToKeep :: Lude.Maybe Lude.Int,
    licenseType :: Lude.Maybe LicenseType,
    roleName :: Lude.Maybe Lude.Text,
    encrypted :: Lude.Maybe Lude.Bool,
    kmsKeyId :: Lude.Maybe Lude.Text,
    runOnce :: Lude.Maybe Lude.Bool,
    description :: Lude.Maybe Lude.Text,
    serverId :: Lude.Text,
    seedReplicationTime :: Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateReplicationJob' with the minimum fields required to make a request.
--
-- * 'description' - The description of the replication job.
-- * 'encrypted' - Indicates whether the replication job produces encrypted AMIs.
-- * 'frequency' - The time between consecutive replication runs, in hours.
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
-- If encrypted is /true/ but a KMS key ID is not specified, the customer's default KMS key for Amazon EBS is used.
-- * 'licenseType' - The license type to be used for the AMI created by a successful replication run.
-- * 'numberOfRecentAMIsToKeep' - The maximum number of SMS-created AMIs to retain. The oldest is deleted after the maximum number is reached and a new AMI is created.
-- * 'roleName' - The name of the IAM role to be used by the AWS SMS.
-- * 'runOnce' - Indicates whether to run the replication job one time.
-- * 'seedReplicationTime' - The seed replication time.
-- * 'serverId' - The ID of the server.
mkCreateReplicationJob ::
  -- | 'serverId'
  Lude.Text ->
  -- | 'seedReplicationTime'
  Lude.Timestamp ->
  CreateReplicationJob
mkCreateReplicationJob pServerId_ pSeedReplicationTime_ =
  CreateReplicationJob'
    { frequency = Lude.Nothing,
      numberOfRecentAMIsToKeep = Lude.Nothing,
      licenseType = Lude.Nothing,
      roleName = Lude.Nothing,
      encrypted = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      runOnce = Lude.Nothing,
      description = Lude.Nothing,
      serverId = pServerId_,
      seedReplicationTime = pSeedReplicationTime_
    }

-- | The time between consecutive replication runs, in hours.
--
-- /Note:/ Consider using 'frequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crjFrequency :: Lens.Lens' CreateReplicationJob (Lude.Maybe Lude.Int)
crjFrequency = Lens.lens (frequency :: CreateReplicationJob -> Lude.Maybe Lude.Int) (\s a -> s {frequency = a} :: CreateReplicationJob)
{-# DEPRECATED crjFrequency "Use generic-lens or generic-optics with 'frequency' instead." #-}

-- | The maximum number of SMS-created AMIs to retain. The oldest is deleted after the maximum number is reached and a new AMI is created.
--
-- /Note:/ Consider using 'numberOfRecentAMIsToKeep' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crjNumberOfRecentAMIsToKeep :: Lens.Lens' CreateReplicationJob (Lude.Maybe Lude.Int)
crjNumberOfRecentAMIsToKeep = Lens.lens (numberOfRecentAMIsToKeep :: CreateReplicationJob -> Lude.Maybe Lude.Int) (\s a -> s {numberOfRecentAMIsToKeep = a} :: CreateReplicationJob)
{-# DEPRECATED crjNumberOfRecentAMIsToKeep "Use generic-lens or generic-optics with 'numberOfRecentAMIsToKeep' instead." #-}

-- | The license type to be used for the AMI created by a successful replication run.
--
-- /Note:/ Consider using 'licenseType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crjLicenseType :: Lens.Lens' CreateReplicationJob (Lude.Maybe LicenseType)
crjLicenseType = Lens.lens (licenseType :: CreateReplicationJob -> Lude.Maybe LicenseType) (\s a -> s {licenseType = a} :: CreateReplicationJob)
{-# DEPRECATED crjLicenseType "Use generic-lens or generic-optics with 'licenseType' instead." #-}

-- | The name of the IAM role to be used by the AWS SMS.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crjRoleName :: Lens.Lens' CreateReplicationJob (Lude.Maybe Lude.Text)
crjRoleName = Lens.lens (roleName :: CreateReplicationJob -> Lude.Maybe Lude.Text) (\s a -> s {roleName = a} :: CreateReplicationJob)
{-# DEPRECATED crjRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | Indicates whether the replication job produces encrypted AMIs.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crjEncrypted :: Lens.Lens' CreateReplicationJob (Lude.Maybe Lude.Bool)
crjEncrypted = Lens.lens (encrypted :: CreateReplicationJob -> Lude.Maybe Lude.Bool) (\s a -> s {encrypted = a} :: CreateReplicationJob)
{-# DEPRECATED crjEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

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
crjKmsKeyId :: Lens.Lens' CreateReplicationJob (Lude.Maybe Lude.Text)
crjKmsKeyId = Lens.lens (kmsKeyId :: CreateReplicationJob -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: CreateReplicationJob)
{-# DEPRECATED crjKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | Indicates whether to run the replication job one time.
--
-- /Note:/ Consider using 'runOnce' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crjRunOnce :: Lens.Lens' CreateReplicationJob (Lude.Maybe Lude.Bool)
crjRunOnce = Lens.lens (runOnce :: CreateReplicationJob -> Lude.Maybe Lude.Bool) (\s a -> s {runOnce = a} :: CreateReplicationJob)
{-# DEPRECATED crjRunOnce "Use generic-lens or generic-optics with 'runOnce' instead." #-}

-- | The description of the replication job.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crjDescription :: Lens.Lens' CreateReplicationJob (Lude.Maybe Lude.Text)
crjDescription = Lens.lens (description :: CreateReplicationJob -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateReplicationJob)
{-# DEPRECATED crjDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ID of the server.
--
-- /Note:/ Consider using 'serverId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crjServerId :: Lens.Lens' CreateReplicationJob Lude.Text
crjServerId = Lens.lens (serverId :: CreateReplicationJob -> Lude.Text) (\s a -> s {serverId = a} :: CreateReplicationJob)
{-# DEPRECATED crjServerId "Use generic-lens or generic-optics with 'serverId' instead." #-}

-- | The seed replication time.
--
-- /Note:/ Consider using 'seedReplicationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crjSeedReplicationTime :: Lens.Lens' CreateReplicationJob Lude.Timestamp
crjSeedReplicationTime = Lens.lens (seedReplicationTime :: CreateReplicationJob -> Lude.Timestamp) (\s a -> s {seedReplicationTime = a} :: CreateReplicationJob)
{-# DEPRECATED crjSeedReplicationTime "Use generic-lens or generic-optics with 'seedReplicationTime' instead." #-}

instance Lude.AWSRequest CreateReplicationJob where
  type Rs CreateReplicationJob = CreateReplicationJobResponse
  request = Req.postJSON smsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateReplicationJobResponse'
            Lude.<$> (x Lude..?> "replicationJobId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateReplicationJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSServerMigrationService_V2016_10_24.CreateReplicationJob" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateReplicationJob where
  toJSON CreateReplicationJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("frequency" Lude..=) Lude.<$> frequency,
            ("numberOfRecentAmisToKeep" Lude..=)
              Lude.<$> numberOfRecentAMIsToKeep,
            ("licenseType" Lude..=) Lude.<$> licenseType,
            ("roleName" Lude..=) Lude.<$> roleName,
            ("encrypted" Lude..=) Lude.<$> encrypted,
            ("kmsKeyId" Lude..=) Lude.<$> kmsKeyId,
            ("runOnce" Lude..=) Lude.<$> runOnce,
            ("description" Lude..=) Lude.<$> description,
            Lude.Just ("serverId" Lude..= serverId),
            Lude.Just ("seedReplicationTime" Lude..= seedReplicationTime)
          ]
      )

instance Lude.ToPath CreateReplicationJob where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateReplicationJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateReplicationJobResponse' smart constructor.
data CreateReplicationJobResponse = CreateReplicationJobResponse'
  { replicationJobId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateReplicationJobResponse' with the minimum fields required to make a request.
--
-- * 'replicationJobId' - The unique identifier of the replication job.
-- * 'responseStatus' - The response status code.
mkCreateReplicationJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateReplicationJobResponse
mkCreateReplicationJobResponse pResponseStatus_ =
  CreateReplicationJobResponse'
    { replicationJobId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique identifier of the replication job.
--
-- /Note:/ Consider using 'replicationJobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crjrsReplicationJobId :: Lens.Lens' CreateReplicationJobResponse (Lude.Maybe Lude.Text)
crjrsReplicationJobId = Lens.lens (replicationJobId :: CreateReplicationJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {replicationJobId = a} :: CreateReplicationJobResponse)
{-# DEPRECATED crjrsReplicationJobId "Use generic-lens or generic-optics with 'replicationJobId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crjrsResponseStatus :: Lens.Lens' CreateReplicationJobResponse Lude.Int
crjrsResponseStatus = Lens.lens (responseStatus :: CreateReplicationJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateReplicationJobResponse)
{-# DEPRECATED crjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
