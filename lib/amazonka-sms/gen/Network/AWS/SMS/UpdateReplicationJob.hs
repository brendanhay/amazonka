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
-- Module      : Network.AWS.SMS.UpdateReplicationJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified settings for the specified replication job.
module Network.AWS.SMS.UpdateReplicationJob
  ( -- * Creating a Request
    updateReplicationJob,
    UpdateReplicationJob,

    -- * Request Lenses
    urjFrequency,
    urjNumberOfRecentAMIsToKeep,
    urjLicenseType,
    urjRoleName,
    urjEncrypted,
    urjNextReplicationRunStartTime,
    urjKmsKeyId,
    urjDescription,
    urjReplicationJobId,

    -- * Destructuring the Response
    updateReplicationJobResponse,
    UpdateReplicationJobResponse,

    -- * Response Lenses
    urjrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SMS.Types

-- | /See:/ 'updateReplicationJob' smart constructor.
data UpdateReplicationJob = UpdateReplicationJob'
  { _urjFrequency ::
      !(Maybe Int),
    _urjNumberOfRecentAMIsToKeep :: !(Maybe Int),
    _urjLicenseType :: !(Maybe LicenseType),
    _urjRoleName :: !(Maybe Text),
    _urjEncrypted :: !(Maybe Bool),
    _urjNextReplicationRunStartTime :: !(Maybe POSIX),
    _urjKmsKeyId :: !(Maybe Text),
    _urjDescription :: !(Maybe Text),
    _urjReplicationJobId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateReplicationJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urjFrequency' - The time between consecutive replication runs, in hours.
--
-- * 'urjNumberOfRecentAMIsToKeep' - The maximum number of SMS-created AMIs to retain. The oldest is deleted after the maximum number is reached and a new AMI is created.
--
-- * 'urjLicenseType' - The license type to be used for the AMI created by a successful replication run.
--
-- * 'urjRoleName' - The name of the IAM role to be used by AWS SMS.
--
-- * 'urjEncrypted' - When true, the replication job produces encrypted AMIs. For more information, @KmsKeyId@ .
--
-- * 'urjNextReplicationRunStartTime' - The start time of the next replication run.
--
-- * 'urjKmsKeyId' - The ID of the KMS key for replication jobs that produce encrypted AMIs. This value can be any of the following:     * KMS key ID     * KMS key alias     * ARN referring to the KMS key ID     * ARN referring to the KMS key alias If encrypted is enabled but a KMS key ID is not specified, the customer's default KMS key for Amazon EBS is used.
--
-- * 'urjDescription' - The description of the replication job.
--
-- * 'urjReplicationJobId' - The ID of the replication job.
updateReplicationJob ::
  -- | 'urjReplicationJobId'
  Text ->
  UpdateReplicationJob
updateReplicationJob pReplicationJobId_ =
  UpdateReplicationJob'
    { _urjFrequency = Nothing,
      _urjNumberOfRecentAMIsToKeep = Nothing,
      _urjLicenseType = Nothing,
      _urjRoleName = Nothing,
      _urjEncrypted = Nothing,
      _urjNextReplicationRunStartTime = Nothing,
      _urjKmsKeyId = Nothing,
      _urjDescription = Nothing,
      _urjReplicationJobId = pReplicationJobId_
    }

-- | The time between consecutive replication runs, in hours.
urjFrequency :: Lens' UpdateReplicationJob (Maybe Int)
urjFrequency = lens _urjFrequency (\s a -> s {_urjFrequency = a})

-- | The maximum number of SMS-created AMIs to retain. The oldest is deleted after the maximum number is reached and a new AMI is created.
urjNumberOfRecentAMIsToKeep :: Lens' UpdateReplicationJob (Maybe Int)
urjNumberOfRecentAMIsToKeep = lens _urjNumberOfRecentAMIsToKeep (\s a -> s {_urjNumberOfRecentAMIsToKeep = a})

-- | The license type to be used for the AMI created by a successful replication run.
urjLicenseType :: Lens' UpdateReplicationJob (Maybe LicenseType)
urjLicenseType = lens _urjLicenseType (\s a -> s {_urjLicenseType = a})

-- | The name of the IAM role to be used by AWS SMS.
urjRoleName :: Lens' UpdateReplicationJob (Maybe Text)
urjRoleName = lens _urjRoleName (\s a -> s {_urjRoleName = a})

-- | When true, the replication job produces encrypted AMIs. For more information, @KmsKeyId@ .
urjEncrypted :: Lens' UpdateReplicationJob (Maybe Bool)
urjEncrypted = lens _urjEncrypted (\s a -> s {_urjEncrypted = a})

-- | The start time of the next replication run.
urjNextReplicationRunStartTime :: Lens' UpdateReplicationJob (Maybe UTCTime)
urjNextReplicationRunStartTime = lens _urjNextReplicationRunStartTime (\s a -> s {_urjNextReplicationRunStartTime = a}) . mapping _Time

-- | The ID of the KMS key for replication jobs that produce encrypted AMIs. This value can be any of the following:     * KMS key ID     * KMS key alias     * ARN referring to the KMS key ID     * ARN referring to the KMS key alias If encrypted is enabled but a KMS key ID is not specified, the customer's default KMS key for Amazon EBS is used.
urjKmsKeyId :: Lens' UpdateReplicationJob (Maybe Text)
urjKmsKeyId = lens _urjKmsKeyId (\s a -> s {_urjKmsKeyId = a})

-- | The description of the replication job.
urjDescription :: Lens' UpdateReplicationJob (Maybe Text)
urjDescription = lens _urjDescription (\s a -> s {_urjDescription = a})

-- | The ID of the replication job.
urjReplicationJobId :: Lens' UpdateReplicationJob Text
urjReplicationJobId = lens _urjReplicationJobId (\s a -> s {_urjReplicationJobId = a})

instance AWSRequest UpdateReplicationJob where
  type Rs UpdateReplicationJob = UpdateReplicationJobResponse
  request = postJSON sms
  response =
    receiveEmpty
      (\s h x -> UpdateReplicationJobResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateReplicationJob

instance NFData UpdateReplicationJob

instance ToHeaders UpdateReplicationJob where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSServerMigrationService_V2016_10_24.UpdateReplicationJob" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateReplicationJob where
  toJSON UpdateReplicationJob' {..} =
    object
      ( catMaybes
          [ ("frequency" .=) <$> _urjFrequency,
            ("numberOfRecentAmisToKeep" .=) <$> _urjNumberOfRecentAMIsToKeep,
            ("licenseType" .=) <$> _urjLicenseType,
            ("roleName" .=) <$> _urjRoleName,
            ("encrypted" .=) <$> _urjEncrypted,
            ("nextReplicationRunStartTime" .=)
              <$> _urjNextReplicationRunStartTime,
            ("kmsKeyId" .=) <$> _urjKmsKeyId,
            ("description" .=) <$> _urjDescription,
            Just ("replicationJobId" .= _urjReplicationJobId)
          ]
      )

instance ToPath UpdateReplicationJob where
  toPath = const "/"

instance ToQuery UpdateReplicationJob where
  toQuery = const mempty

-- | /See:/ 'updateReplicationJobResponse' smart constructor.
newtype UpdateReplicationJobResponse = UpdateReplicationJobResponse'
  { _urjrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateReplicationJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urjrsResponseStatus' - -- | The response status code.
updateReplicationJobResponse ::
  -- | 'urjrsResponseStatus'
  Int ->
  UpdateReplicationJobResponse
updateReplicationJobResponse pResponseStatus_ =
  UpdateReplicationJobResponse'
    { _urjrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
urjrsResponseStatus :: Lens' UpdateReplicationJobResponse Int
urjrsResponseStatus = lens _urjrsResponseStatus (\s a -> s {_urjrsResponseStatus = a})

instance NFData UpdateReplicationJobResponse
