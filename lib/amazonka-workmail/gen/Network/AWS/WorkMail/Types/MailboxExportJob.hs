{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.MailboxExportJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.MailboxExportJob where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WorkMail.Types.MailboxExportJobState

-- | The details of a mailbox export job, including the user or resource ID associated with the mailbox and the S3 bucket that the mailbox contents are exported to.
--
--
--
-- /See:/ 'mailboxExportJob' smart constructor.
data MailboxExportJob = MailboxExportJob'
  { _mejState ::
      !(Maybe MailboxExportJobState),
    _mejJobId :: !(Maybe Text),
    _mejStartTime :: !(Maybe POSIX),
    _mejEstimatedProgress :: !(Maybe Nat),
    _mejEndTime :: !(Maybe POSIX),
    _mejS3Path :: !(Maybe Text),
    _mejEntityId :: !(Maybe Text),
    _mejDescription :: !(Maybe Text),
    _mejS3BucketName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MailboxExportJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mejState' - The state of the mailbox export job.
--
-- * 'mejJobId' - The identifier of the mailbox export job.
--
-- * 'mejStartTime' - The mailbox export job start timestamp.
--
-- * 'mejEstimatedProgress' - The estimated progress of the mailbox export job, in percentage points.
--
-- * 'mejEndTime' - The mailbox export job end timestamp.
--
-- * 'mejS3Path' - The path to the S3 bucket and file that the mailbox export job exports to.
--
-- * 'mejEntityId' - The identifier of the user or resource associated with the mailbox.
--
-- * 'mejDescription' - The mailbox export job description.
--
-- * 'mejS3BucketName' - The name of the S3 bucket.
mailboxExportJob ::
  MailboxExportJob
mailboxExportJob =
  MailboxExportJob'
    { _mejState = Nothing,
      _mejJobId = Nothing,
      _mejStartTime = Nothing,
      _mejEstimatedProgress = Nothing,
      _mejEndTime = Nothing,
      _mejS3Path = Nothing,
      _mejEntityId = Nothing,
      _mejDescription = Nothing,
      _mejS3BucketName = Nothing
    }

-- | The state of the mailbox export job.
mejState :: Lens' MailboxExportJob (Maybe MailboxExportJobState)
mejState = lens _mejState (\s a -> s {_mejState = a})

-- | The identifier of the mailbox export job.
mejJobId :: Lens' MailboxExportJob (Maybe Text)
mejJobId = lens _mejJobId (\s a -> s {_mejJobId = a})

-- | The mailbox export job start timestamp.
mejStartTime :: Lens' MailboxExportJob (Maybe UTCTime)
mejStartTime = lens _mejStartTime (\s a -> s {_mejStartTime = a}) . mapping _Time

-- | The estimated progress of the mailbox export job, in percentage points.
mejEstimatedProgress :: Lens' MailboxExportJob (Maybe Natural)
mejEstimatedProgress = lens _mejEstimatedProgress (\s a -> s {_mejEstimatedProgress = a}) . mapping _Nat

-- | The mailbox export job end timestamp.
mejEndTime :: Lens' MailboxExportJob (Maybe UTCTime)
mejEndTime = lens _mejEndTime (\s a -> s {_mejEndTime = a}) . mapping _Time

-- | The path to the S3 bucket and file that the mailbox export job exports to.
mejS3Path :: Lens' MailboxExportJob (Maybe Text)
mejS3Path = lens _mejS3Path (\s a -> s {_mejS3Path = a})

-- | The identifier of the user or resource associated with the mailbox.
mejEntityId :: Lens' MailboxExportJob (Maybe Text)
mejEntityId = lens _mejEntityId (\s a -> s {_mejEntityId = a})

-- | The mailbox export job description.
mejDescription :: Lens' MailboxExportJob (Maybe Text)
mejDescription = lens _mejDescription (\s a -> s {_mejDescription = a})

-- | The name of the S3 bucket.
mejS3BucketName :: Lens' MailboxExportJob (Maybe Text)
mejS3BucketName = lens _mejS3BucketName (\s a -> s {_mejS3BucketName = a})

instance FromJSON MailboxExportJob where
  parseJSON =
    withObject
      "MailboxExportJob"
      ( \x ->
          MailboxExportJob'
            <$> (x .:? "State")
            <*> (x .:? "JobId")
            <*> (x .:? "StartTime")
            <*> (x .:? "EstimatedProgress")
            <*> (x .:? "EndTime")
            <*> (x .:? "S3Path")
            <*> (x .:? "EntityId")
            <*> (x .:? "Description")
            <*> (x .:? "S3BucketName")
      )

instance Hashable MailboxExportJob

instance NFData MailboxExportJob
