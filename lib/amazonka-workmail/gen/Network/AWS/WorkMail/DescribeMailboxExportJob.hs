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
-- Module      : Network.AWS.WorkMail.DescribeMailboxExportJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the current status of a mailbox export job.
module Network.AWS.WorkMail.DescribeMailboxExportJob
  ( -- * Creating a Request
    describeMailboxExportJob,
    DescribeMailboxExportJob,

    -- * Request Lenses
    dmejJobId,
    dmejOrganizationId,

    -- * Destructuring the Response
    describeMailboxExportJobResponse,
    DescribeMailboxExportJobResponse,

    -- * Response Lenses
    dmejrsState,
    dmejrsKMSKeyARN,
    dmejrsStartTime,
    dmejrsEstimatedProgress,
    dmejrsEndTime,
    dmejrsS3Path,
    dmejrsS3Prefix,
    dmejrsEntityId,
    dmejrsDescription,
    dmejrsErrorInfo,
    dmejrsS3BucketName,
    dmejrsRoleARN,
    dmejrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'describeMailboxExportJob' smart constructor.
data DescribeMailboxExportJob = DescribeMailboxExportJob'
  { _dmejJobId ::
      !Text,
    _dmejOrganizationId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeMailboxExportJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmejJobId' - The mailbox export job ID.
--
-- * 'dmejOrganizationId' - The organization ID.
describeMailboxExportJob ::
  -- | 'dmejJobId'
  Text ->
  -- | 'dmejOrganizationId'
  Text ->
  DescribeMailboxExportJob
describeMailboxExportJob pJobId_ pOrganizationId_ =
  DescribeMailboxExportJob'
    { _dmejJobId = pJobId_,
      _dmejOrganizationId = pOrganizationId_
    }

-- | The mailbox export job ID.
dmejJobId :: Lens' DescribeMailboxExportJob Text
dmejJobId = lens _dmejJobId (\s a -> s {_dmejJobId = a})

-- | The organization ID.
dmejOrganizationId :: Lens' DescribeMailboxExportJob Text
dmejOrganizationId = lens _dmejOrganizationId (\s a -> s {_dmejOrganizationId = a})

instance AWSRequest DescribeMailboxExportJob where
  type Rs DescribeMailboxExportJob = DescribeMailboxExportJobResponse
  request = postJSON workMail
  response =
    receiveJSON
      ( \s h x ->
          DescribeMailboxExportJobResponse'
            <$> (x .?> "State")
            <*> (x .?> "KmsKeyArn")
            <*> (x .?> "StartTime")
            <*> (x .?> "EstimatedProgress")
            <*> (x .?> "EndTime")
            <*> (x .?> "S3Path")
            <*> (x .?> "S3Prefix")
            <*> (x .?> "EntityId")
            <*> (x .?> "Description")
            <*> (x .?> "ErrorInfo")
            <*> (x .?> "S3BucketName")
            <*> (x .?> "RoleArn")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeMailboxExportJob

instance NFData DescribeMailboxExportJob

instance ToHeaders DescribeMailboxExportJob where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("WorkMailService.DescribeMailboxExportJob" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeMailboxExportJob where
  toJSON DescribeMailboxExportJob' {..} =
    object
      ( catMaybes
          [ Just ("JobId" .= _dmejJobId),
            Just ("OrganizationId" .= _dmejOrganizationId)
          ]
      )

instance ToPath DescribeMailboxExportJob where
  toPath = const "/"

instance ToQuery DescribeMailboxExportJob where
  toQuery = const mempty

-- | /See:/ 'describeMailboxExportJobResponse' smart constructor.
data DescribeMailboxExportJobResponse = DescribeMailboxExportJobResponse'
  { _dmejrsState ::
      !( Maybe
           MailboxExportJobState
       ),
    _dmejrsKMSKeyARN ::
      !(Maybe Text),
    _dmejrsStartTime ::
      !(Maybe POSIX),
    _dmejrsEstimatedProgress ::
      !(Maybe Nat),
    _dmejrsEndTime ::
      !(Maybe POSIX),
    _dmejrsS3Path ::
      !(Maybe Text),
    _dmejrsS3Prefix ::
      !(Maybe Text),
    _dmejrsEntityId ::
      !(Maybe Text),
    _dmejrsDescription ::
      !(Maybe Text),
    _dmejrsErrorInfo ::
      !(Maybe Text),
    _dmejrsS3BucketName ::
      !(Maybe Text),
    _dmejrsRoleARN ::
      !(Maybe Text),
    _dmejrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeMailboxExportJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmejrsState' - The state of the mailbox export job.
--
-- * 'dmejrsKMSKeyARN' - The Amazon Resource Name (ARN) of the symmetric AWS Key Management Service (AWS KMS) key that encrypts the exported mailbox content.
--
-- * 'dmejrsStartTime' - The mailbox export job start timestamp.
--
-- * 'dmejrsEstimatedProgress' - The estimated progress of the mailbox export job, in percentage points.
--
-- * 'dmejrsEndTime' - The mailbox export job end timestamp.
--
-- * 'dmejrsS3Path' - The path to the S3 bucket and file that the mailbox export job is exporting to.
--
-- * 'dmejrsS3Prefix' - The S3 bucket prefix.
--
-- * 'dmejrsEntityId' - The identifier of the user or resource associated with the mailbox.
--
-- * 'dmejrsDescription' - The mailbox export job description.
--
-- * 'dmejrsErrorInfo' - Error information for failed mailbox export jobs.
--
-- * 'dmejrsS3BucketName' - The name of the S3 bucket.
--
-- * 'dmejrsRoleARN' - The ARN of the AWS Identity and Access Management (IAM) role that grants write permission to the Amazon Simple Storage Service (Amazon S3) bucket.
--
-- * 'dmejrsResponseStatus' - -- | The response status code.
describeMailboxExportJobResponse ::
  -- | 'dmejrsResponseStatus'
  Int ->
  DescribeMailboxExportJobResponse
describeMailboxExportJobResponse pResponseStatus_ =
  DescribeMailboxExportJobResponse'
    { _dmejrsState = Nothing,
      _dmejrsKMSKeyARN = Nothing,
      _dmejrsStartTime = Nothing,
      _dmejrsEstimatedProgress = Nothing,
      _dmejrsEndTime = Nothing,
      _dmejrsS3Path = Nothing,
      _dmejrsS3Prefix = Nothing,
      _dmejrsEntityId = Nothing,
      _dmejrsDescription = Nothing,
      _dmejrsErrorInfo = Nothing,
      _dmejrsS3BucketName = Nothing,
      _dmejrsRoleARN = Nothing,
      _dmejrsResponseStatus = pResponseStatus_
    }

-- | The state of the mailbox export job.
dmejrsState :: Lens' DescribeMailboxExportJobResponse (Maybe MailboxExportJobState)
dmejrsState = lens _dmejrsState (\s a -> s {_dmejrsState = a})

-- | The Amazon Resource Name (ARN) of the symmetric AWS Key Management Service (AWS KMS) key that encrypts the exported mailbox content.
dmejrsKMSKeyARN :: Lens' DescribeMailboxExportJobResponse (Maybe Text)
dmejrsKMSKeyARN = lens _dmejrsKMSKeyARN (\s a -> s {_dmejrsKMSKeyARN = a})

-- | The mailbox export job start timestamp.
dmejrsStartTime :: Lens' DescribeMailboxExportJobResponse (Maybe UTCTime)
dmejrsStartTime = lens _dmejrsStartTime (\s a -> s {_dmejrsStartTime = a}) . mapping _Time

-- | The estimated progress of the mailbox export job, in percentage points.
dmejrsEstimatedProgress :: Lens' DescribeMailboxExportJobResponse (Maybe Natural)
dmejrsEstimatedProgress = lens _dmejrsEstimatedProgress (\s a -> s {_dmejrsEstimatedProgress = a}) . mapping _Nat

-- | The mailbox export job end timestamp.
dmejrsEndTime :: Lens' DescribeMailboxExportJobResponse (Maybe UTCTime)
dmejrsEndTime = lens _dmejrsEndTime (\s a -> s {_dmejrsEndTime = a}) . mapping _Time

-- | The path to the S3 bucket and file that the mailbox export job is exporting to.
dmejrsS3Path :: Lens' DescribeMailboxExportJobResponse (Maybe Text)
dmejrsS3Path = lens _dmejrsS3Path (\s a -> s {_dmejrsS3Path = a})

-- | The S3 bucket prefix.
dmejrsS3Prefix :: Lens' DescribeMailboxExportJobResponse (Maybe Text)
dmejrsS3Prefix = lens _dmejrsS3Prefix (\s a -> s {_dmejrsS3Prefix = a})

-- | The identifier of the user or resource associated with the mailbox.
dmejrsEntityId :: Lens' DescribeMailboxExportJobResponse (Maybe Text)
dmejrsEntityId = lens _dmejrsEntityId (\s a -> s {_dmejrsEntityId = a})

-- | The mailbox export job description.
dmejrsDescription :: Lens' DescribeMailboxExportJobResponse (Maybe Text)
dmejrsDescription = lens _dmejrsDescription (\s a -> s {_dmejrsDescription = a})

-- | Error information for failed mailbox export jobs.
dmejrsErrorInfo :: Lens' DescribeMailboxExportJobResponse (Maybe Text)
dmejrsErrorInfo = lens _dmejrsErrorInfo (\s a -> s {_dmejrsErrorInfo = a})

-- | The name of the S3 bucket.
dmejrsS3BucketName :: Lens' DescribeMailboxExportJobResponse (Maybe Text)
dmejrsS3BucketName = lens _dmejrsS3BucketName (\s a -> s {_dmejrsS3BucketName = a})

-- | The ARN of the AWS Identity and Access Management (IAM) role that grants write permission to the Amazon Simple Storage Service (Amazon S3) bucket.
dmejrsRoleARN :: Lens' DescribeMailboxExportJobResponse (Maybe Text)
dmejrsRoleARN = lens _dmejrsRoleARN (\s a -> s {_dmejrsRoleARN = a})

-- | -- | The response status code.
dmejrsResponseStatus :: Lens' DescribeMailboxExportJobResponse Int
dmejrsResponseStatus = lens _dmejrsResponseStatus (\s a -> s {_dmejrsResponseStatus = a})

instance NFData DescribeMailboxExportJobResponse
