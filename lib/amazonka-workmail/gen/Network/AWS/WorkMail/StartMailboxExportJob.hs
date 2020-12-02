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
-- Module      : Network.AWS.WorkMail.StartMailboxExportJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a mailbox export job to export MIME-format email messages and calendar items from the specified mailbox to the specified Amazon Simple Storage Service (Amazon S3) bucket. For more information, see <https://docs.aws.amazon.com/workmail/latest/adminguide/mail-export.html Exporting mailbox content> in the /Amazon WorkMail Administrator Guide/ .
module Network.AWS.WorkMail.StartMailboxExportJob
  ( -- * Creating a Request
    startMailboxExportJob,
    StartMailboxExportJob,

    -- * Request Lenses
    smejDescription,
    smejClientToken,
    smejOrganizationId,
    smejEntityId,
    smejRoleARN,
    smejKMSKeyARN,
    smejS3BucketName,
    smejS3Prefix,

    -- * Destructuring the Response
    startMailboxExportJobResponse,
    StartMailboxExportJobResponse,

    -- * Response Lenses
    smejrsJobId,
    smejrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'startMailboxExportJob' smart constructor.
data StartMailboxExportJob = StartMailboxExportJob'
  { _smejDescription ::
      !(Maybe Text),
    _smejClientToken :: !Text,
    _smejOrganizationId :: !Text,
    _smejEntityId :: !Text,
    _smejRoleARN :: !Text,
    _smejKMSKeyARN :: !Text,
    _smejS3BucketName :: !Text,
    _smejS3Prefix :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartMailboxExportJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smejDescription' - The mailbox export job description.
--
-- * 'smejClientToken' - The idempotency token for the client request.
--
-- * 'smejOrganizationId' - The identifier associated with the organization.
--
-- * 'smejEntityId' - The identifier of the user or resource associated with the mailbox.
--
-- * 'smejRoleARN' - The ARN of the AWS Identity and Access Management (IAM) role that grants write permission to the S3 bucket.
--
-- * 'smejKMSKeyARN' - The Amazon Resource Name (ARN) of the symmetric AWS Key Management Service (AWS KMS) key that encrypts the exported mailbox content.
--
-- * 'smejS3BucketName' - The name of the S3 bucket.
--
-- * 'smejS3Prefix' - The S3 bucket prefix.
startMailboxExportJob ::
  -- | 'smejClientToken'
  Text ->
  -- | 'smejOrganizationId'
  Text ->
  -- | 'smejEntityId'
  Text ->
  -- | 'smejRoleARN'
  Text ->
  -- | 'smejKMSKeyARN'
  Text ->
  -- | 'smejS3BucketName'
  Text ->
  -- | 'smejS3Prefix'
  Text ->
  StartMailboxExportJob
startMailboxExportJob
  pClientToken_
  pOrganizationId_
  pEntityId_
  pRoleARN_
  pKMSKeyARN_
  pS3BucketName_
  pS3Prefix_ =
    StartMailboxExportJob'
      { _smejDescription = Nothing,
        _smejClientToken = pClientToken_,
        _smejOrganizationId = pOrganizationId_,
        _smejEntityId = pEntityId_,
        _smejRoleARN = pRoleARN_,
        _smejKMSKeyARN = pKMSKeyARN_,
        _smejS3BucketName = pS3BucketName_,
        _smejS3Prefix = pS3Prefix_
      }

-- | The mailbox export job description.
smejDescription :: Lens' StartMailboxExportJob (Maybe Text)
smejDescription = lens _smejDescription (\s a -> s {_smejDescription = a})

-- | The idempotency token for the client request.
smejClientToken :: Lens' StartMailboxExportJob Text
smejClientToken = lens _smejClientToken (\s a -> s {_smejClientToken = a})

-- | The identifier associated with the organization.
smejOrganizationId :: Lens' StartMailboxExportJob Text
smejOrganizationId = lens _smejOrganizationId (\s a -> s {_smejOrganizationId = a})

-- | The identifier of the user or resource associated with the mailbox.
smejEntityId :: Lens' StartMailboxExportJob Text
smejEntityId = lens _smejEntityId (\s a -> s {_smejEntityId = a})

-- | The ARN of the AWS Identity and Access Management (IAM) role that grants write permission to the S3 bucket.
smejRoleARN :: Lens' StartMailboxExportJob Text
smejRoleARN = lens _smejRoleARN (\s a -> s {_smejRoleARN = a})

-- | The Amazon Resource Name (ARN) of the symmetric AWS Key Management Service (AWS KMS) key that encrypts the exported mailbox content.
smejKMSKeyARN :: Lens' StartMailboxExportJob Text
smejKMSKeyARN = lens _smejKMSKeyARN (\s a -> s {_smejKMSKeyARN = a})

-- | The name of the S3 bucket.
smejS3BucketName :: Lens' StartMailboxExportJob Text
smejS3BucketName = lens _smejS3BucketName (\s a -> s {_smejS3BucketName = a})

-- | The S3 bucket prefix.
smejS3Prefix :: Lens' StartMailboxExportJob Text
smejS3Prefix = lens _smejS3Prefix (\s a -> s {_smejS3Prefix = a})

instance AWSRequest StartMailboxExportJob where
  type Rs StartMailboxExportJob = StartMailboxExportJobResponse
  request = postJSON workMail
  response =
    receiveJSON
      ( \s h x ->
          StartMailboxExportJobResponse'
            <$> (x .?> "JobId") <*> (pure (fromEnum s))
      )

instance Hashable StartMailboxExportJob

instance NFData StartMailboxExportJob

instance ToHeaders StartMailboxExportJob where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("WorkMailService.StartMailboxExportJob" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StartMailboxExportJob where
  toJSON StartMailboxExportJob' {..} =
    object
      ( catMaybes
          [ ("Description" .=) <$> _smejDescription,
            Just ("ClientToken" .= _smejClientToken),
            Just ("OrganizationId" .= _smejOrganizationId),
            Just ("EntityId" .= _smejEntityId),
            Just ("RoleArn" .= _smejRoleARN),
            Just ("KmsKeyArn" .= _smejKMSKeyARN),
            Just ("S3BucketName" .= _smejS3BucketName),
            Just ("S3Prefix" .= _smejS3Prefix)
          ]
      )

instance ToPath StartMailboxExportJob where
  toPath = const "/"

instance ToQuery StartMailboxExportJob where
  toQuery = const mempty

-- | /See:/ 'startMailboxExportJobResponse' smart constructor.
data StartMailboxExportJobResponse = StartMailboxExportJobResponse'
  { _smejrsJobId ::
      !(Maybe Text),
    _smejrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartMailboxExportJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smejrsJobId' - The job ID.
--
-- * 'smejrsResponseStatus' - -- | The response status code.
startMailboxExportJobResponse ::
  -- | 'smejrsResponseStatus'
  Int ->
  StartMailboxExportJobResponse
startMailboxExportJobResponse pResponseStatus_ =
  StartMailboxExportJobResponse'
    { _smejrsJobId = Nothing,
      _smejrsResponseStatus = pResponseStatus_
    }

-- | The job ID.
smejrsJobId :: Lens' StartMailboxExportJobResponse (Maybe Text)
smejrsJobId = lens _smejrsJobId (\s a -> s {_smejrsJobId = a})

-- | -- | The response status code.
smejrsResponseStatus :: Lens' StartMailboxExportJobResponse Int
smejrsResponseStatus = lens _smejrsResponseStatus (\s a -> s {_smejrsResponseStatus = a})

instance NFData StartMailboxExportJobResponse
