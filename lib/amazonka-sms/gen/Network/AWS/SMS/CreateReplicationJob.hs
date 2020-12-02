{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
  ( -- * Creating a Request
    createReplicationJob,
    CreateReplicationJob,

    -- * Request Lenses
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

    -- * Destructuring the Response
    createReplicationJobResponse,
    CreateReplicationJobResponse,

    -- * Response Lenses
    crjrsReplicationJobId,
    crjrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SMS.Types

-- | /See:/ 'createReplicationJob' smart constructor.
data CreateReplicationJob = CreateReplicationJob'
  { _crjFrequency ::
      !(Maybe Int),
    _crjNumberOfRecentAMIsToKeep :: !(Maybe Int),
    _crjLicenseType :: !(Maybe LicenseType),
    _crjRoleName :: !(Maybe Text),
    _crjEncrypted :: !(Maybe Bool),
    _crjKmsKeyId :: !(Maybe Text),
    _crjRunOnce :: !(Maybe Bool),
    _crjDescription :: !(Maybe Text),
    _crjServerId :: !Text,
    _crjSeedReplicationTime :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateReplicationJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crjFrequency' - The time between consecutive replication runs, in hours.
--
-- * 'crjNumberOfRecentAMIsToKeep' - The maximum number of SMS-created AMIs to retain. The oldest is deleted after the maximum number is reached and a new AMI is created.
--
-- * 'crjLicenseType' - The license type to be used for the AMI created by a successful replication run.
--
-- * 'crjRoleName' - The name of the IAM role to be used by the AWS SMS.
--
-- * 'crjEncrypted' - Indicates whether the replication job produces encrypted AMIs.
--
-- * 'crjKmsKeyId' - The ID of the KMS key for replication jobs that produce encrypted AMIs. This value can be any of the following:     * KMS key ID     * KMS key alias     * ARN referring to the KMS key ID     * ARN referring to the KMS key alias If encrypted is /true/ but a KMS key ID is not specified, the customer's default KMS key for Amazon EBS is used.
--
-- * 'crjRunOnce' - Indicates whether to run the replication job one time.
--
-- * 'crjDescription' - The description of the replication job.
--
-- * 'crjServerId' - The ID of the server.
--
-- * 'crjSeedReplicationTime' - The seed replication time.
createReplicationJob ::
  -- | 'crjServerId'
  Text ->
  -- | 'crjSeedReplicationTime'
  UTCTime ->
  CreateReplicationJob
createReplicationJob pServerId_ pSeedReplicationTime_ =
  CreateReplicationJob'
    { _crjFrequency = Nothing,
      _crjNumberOfRecentAMIsToKeep = Nothing,
      _crjLicenseType = Nothing,
      _crjRoleName = Nothing,
      _crjEncrypted = Nothing,
      _crjKmsKeyId = Nothing,
      _crjRunOnce = Nothing,
      _crjDescription = Nothing,
      _crjServerId = pServerId_,
      _crjSeedReplicationTime = _Time # pSeedReplicationTime_
    }

-- | The time between consecutive replication runs, in hours.
crjFrequency :: Lens' CreateReplicationJob (Maybe Int)
crjFrequency = lens _crjFrequency (\s a -> s {_crjFrequency = a})

-- | The maximum number of SMS-created AMIs to retain. The oldest is deleted after the maximum number is reached and a new AMI is created.
crjNumberOfRecentAMIsToKeep :: Lens' CreateReplicationJob (Maybe Int)
crjNumberOfRecentAMIsToKeep = lens _crjNumberOfRecentAMIsToKeep (\s a -> s {_crjNumberOfRecentAMIsToKeep = a})

-- | The license type to be used for the AMI created by a successful replication run.
crjLicenseType :: Lens' CreateReplicationJob (Maybe LicenseType)
crjLicenseType = lens _crjLicenseType (\s a -> s {_crjLicenseType = a})

-- | The name of the IAM role to be used by the AWS SMS.
crjRoleName :: Lens' CreateReplicationJob (Maybe Text)
crjRoleName = lens _crjRoleName (\s a -> s {_crjRoleName = a})

-- | Indicates whether the replication job produces encrypted AMIs.
crjEncrypted :: Lens' CreateReplicationJob (Maybe Bool)
crjEncrypted = lens _crjEncrypted (\s a -> s {_crjEncrypted = a})

-- | The ID of the KMS key for replication jobs that produce encrypted AMIs. This value can be any of the following:     * KMS key ID     * KMS key alias     * ARN referring to the KMS key ID     * ARN referring to the KMS key alias If encrypted is /true/ but a KMS key ID is not specified, the customer's default KMS key for Amazon EBS is used.
crjKmsKeyId :: Lens' CreateReplicationJob (Maybe Text)
crjKmsKeyId = lens _crjKmsKeyId (\s a -> s {_crjKmsKeyId = a})

-- | Indicates whether to run the replication job one time.
crjRunOnce :: Lens' CreateReplicationJob (Maybe Bool)
crjRunOnce = lens _crjRunOnce (\s a -> s {_crjRunOnce = a})

-- | The description of the replication job.
crjDescription :: Lens' CreateReplicationJob (Maybe Text)
crjDescription = lens _crjDescription (\s a -> s {_crjDescription = a})

-- | The ID of the server.
crjServerId :: Lens' CreateReplicationJob Text
crjServerId = lens _crjServerId (\s a -> s {_crjServerId = a})

-- | The seed replication time.
crjSeedReplicationTime :: Lens' CreateReplicationJob UTCTime
crjSeedReplicationTime = lens _crjSeedReplicationTime (\s a -> s {_crjSeedReplicationTime = a}) . _Time

instance AWSRequest CreateReplicationJob where
  type Rs CreateReplicationJob = CreateReplicationJobResponse
  request = postJSON sms
  response =
    receiveJSON
      ( \s h x ->
          CreateReplicationJobResponse'
            <$> (x .?> "replicationJobId") <*> (pure (fromEnum s))
      )

instance Hashable CreateReplicationJob

instance NFData CreateReplicationJob

instance ToHeaders CreateReplicationJob where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSServerMigrationService_V2016_10_24.CreateReplicationJob" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateReplicationJob where
  toJSON CreateReplicationJob' {..} =
    object
      ( catMaybes
          [ ("frequency" .=) <$> _crjFrequency,
            ("numberOfRecentAmisToKeep" .=) <$> _crjNumberOfRecentAMIsToKeep,
            ("licenseType" .=) <$> _crjLicenseType,
            ("roleName" .=) <$> _crjRoleName,
            ("encrypted" .=) <$> _crjEncrypted,
            ("kmsKeyId" .=) <$> _crjKmsKeyId,
            ("runOnce" .=) <$> _crjRunOnce,
            ("description" .=) <$> _crjDescription,
            Just ("serverId" .= _crjServerId),
            Just ("seedReplicationTime" .= _crjSeedReplicationTime)
          ]
      )

instance ToPath CreateReplicationJob where
  toPath = const "/"

instance ToQuery CreateReplicationJob where
  toQuery = const mempty

-- | /See:/ 'createReplicationJobResponse' smart constructor.
data CreateReplicationJobResponse = CreateReplicationJobResponse'
  { _crjrsReplicationJobId ::
      !(Maybe Text),
    _crjrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateReplicationJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crjrsReplicationJobId' - The unique identifier of the replication job.
--
-- * 'crjrsResponseStatus' - -- | The response status code.
createReplicationJobResponse ::
  -- | 'crjrsResponseStatus'
  Int ->
  CreateReplicationJobResponse
createReplicationJobResponse pResponseStatus_ =
  CreateReplicationJobResponse'
    { _crjrsReplicationJobId = Nothing,
      _crjrsResponseStatus = pResponseStatus_
    }

-- | The unique identifier of the replication job.
crjrsReplicationJobId :: Lens' CreateReplicationJobResponse (Maybe Text)
crjrsReplicationJobId = lens _crjrsReplicationJobId (\s a -> s {_crjrsReplicationJobId = a})

-- | -- | The response status code.
crjrsResponseStatus :: Lens' CreateReplicationJobResponse Int
crjrsResponseStatus = lens _crjrsResponseStatus (\s a -> s {_crjrsResponseStatus = a})

instance NFData CreateReplicationJobResponse
