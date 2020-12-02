{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.GlacierJobDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.GlacierJobDescription where

import Network.AWS.Glacier.Types.ActionCode
import Network.AWS.Glacier.Types.InventoryRetrievalJobDescription
import Network.AWS.Glacier.Types.OutputLocation
import Network.AWS.Glacier.Types.SelectParameters
import Network.AWS.Glacier.Types.StatusCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the description of an Amazon S3 Glacier job.
--
--
--
-- /See:/ 'glacierJobDescription' smart constructor.
data GlacierJobDescription = GlacierJobDescription'
  { _gjdSHA256TreeHash ::
      !(Maybe Text),
    _gjdArchiveId :: !(Maybe Text),
    _gjdSelectParameters ::
      !(Maybe SelectParameters),
    _gjdJobId :: !(Maybe Text),
    _gjdJobOutputPath :: !(Maybe Text),
    _gjdRetrievalByteRange :: !(Maybe Text),
    _gjdInventoryRetrievalParameters ::
      !(Maybe InventoryRetrievalJobDescription),
    _gjdAction :: !(Maybe ActionCode),
    _gjdJobDescription :: !(Maybe Text),
    _gjdSNSTopic :: !(Maybe Text),
    _gjdStatusMessage :: !(Maybe Text),
    _gjdVaultARN :: !(Maybe Text),
    _gjdOutputLocation :: !(Maybe OutputLocation),
    _gjdTier :: !(Maybe Text),
    _gjdArchiveSHA256TreeHash :: !(Maybe Text),
    _gjdCreationDate :: !(Maybe Text),
    _gjdCompleted :: !(Maybe Bool),
    _gjdCompletionDate :: !(Maybe Text),
    _gjdInventorySizeInBytes :: !(Maybe Integer),
    _gjdArchiveSizeInBytes :: !(Maybe Integer),
    _gjdStatusCode :: !(Maybe StatusCode)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GlacierJobDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gjdSHA256TreeHash' - For an archive retrieval job, this value is the checksum of the archive. Otherwise, this value is null. The SHA256 tree hash value for the requested range of an archive. If the __InitiateJob__ request for an archive specified a tree-hash aligned range, then this field returns a value. If the whole archive is retrieved, this value is the same as the ArchiveSHA256TreeHash value. This field is null for the following:     * Archive retrieval jobs that specify a range that is not tree-hash aligned     * Archival jobs that specify a range that is equal to the whole archive, when the job status is @InProgress@      * Inventory jobs     * Select jobs
--
-- * 'gjdArchiveId' - The archive ID requested for a select job or archive retrieval. Otherwise, this field is null.
--
-- * 'gjdSelectParameters' - Contains the parameters used for a select.
--
-- * 'gjdJobId' - An opaque string that identifies an Amazon S3 Glacier job.
--
-- * 'gjdJobOutputPath' - Contains the job output location.
--
-- * 'gjdRetrievalByteRange' - The retrieved byte range for archive retrieval jobs in the form /StartByteValue/ -/EndByteValue/ . If no range was specified in the archive retrieval, then the whole archive is retrieved. In this case, /StartByteValue/ equals 0 and /EndByteValue/ equals the size of the archive minus 1. For inventory retrieval or select jobs, this field is null.
--
-- * 'gjdInventoryRetrievalParameters' - Parameters used for range inventory retrieval.
--
-- * 'gjdAction' - The job type. This value is either @ArchiveRetrieval@ , @InventoryRetrieval@ , or @Select@ .
--
-- * 'gjdJobDescription' - The job description provided when initiating the job.
--
-- * 'gjdSNSTopic' - An Amazon SNS topic that receives notification.
--
-- * 'gjdStatusMessage' - A friendly message that describes the job status.
--
-- * 'gjdVaultARN' - The Amazon Resource Name (ARN) of the vault from which an archive retrieval was requested.
--
-- * 'gjdOutputLocation' - Contains the location where the data from the select job is stored.
--
-- * 'gjdTier' - The tier to use for a select or an archive retrieval. Valid values are @Expedited@ , @Standard@ , or @Bulk@ . @Standard@ is the default.
--
-- * 'gjdArchiveSHA256TreeHash' - The SHA256 tree hash of the entire archive for an archive retrieval. For inventory retrieval or select jobs, this field is null.
--
-- * 'gjdCreationDate' - The UTC date when the job was created. This value is a string representation of ISO 8601 date format, for example @"2012-03-20T17:03:43.221Z"@ .
--
-- * 'gjdCompleted' - The job status. When a job is completed, you get the job's output using Get Job Output (GET output).
--
-- * 'gjdCompletionDate' - The UTC time that the job request completed. While the job is in progress, the value is null.
--
-- * 'gjdInventorySizeInBytes' - For an inventory retrieval job, this value is the size in bytes of the inventory requested for download. For an archive retrieval or select job, this value is null.
--
-- * 'gjdArchiveSizeInBytes' - For an archive retrieval job, this value is the size in bytes of the archive being requested for download. For an inventory retrieval or select job, this value is null.
--
-- * 'gjdStatusCode' - The status code can be @InProgress@ , @Succeeded@ , or @Failed@ , and indicates the status of the job.
glacierJobDescription ::
  GlacierJobDescription
glacierJobDescription =
  GlacierJobDescription'
    { _gjdSHA256TreeHash = Nothing,
      _gjdArchiveId = Nothing,
      _gjdSelectParameters = Nothing,
      _gjdJobId = Nothing,
      _gjdJobOutputPath = Nothing,
      _gjdRetrievalByteRange = Nothing,
      _gjdInventoryRetrievalParameters = Nothing,
      _gjdAction = Nothing,
      _gjdJobDescription = Nothing,
      _gjdSNSTopic = Nothing,
      _gjdStatusMessage = Nothing,
      _gjdVaultARN = Nothing,
      _gjdOutputLocation = Nothing,
      _gjdTier = Nothing,
      _gjdArchiveSHA256TreeHash = Nothing,
      _gjdCreationDate = Nothing,
      _gjdCompleted = Nothing,
      _gjdCompletionDate = Nothing,
      _gjdInventorySizeInBytes = Nothing,
      _gjdArchiveSizeInBytes = Nothing,
      _gjdStatusCode = Nothing
    }

-- | For an archive retrieval job, this value is the checksum of the archive. Otherwise, this value is null. The SHA256 tree hash value for the requested range of an archive. If the __InitiateJob__ request for an archive specified a tree-hash aligned range, then this field returns a value. If the whole archive is retrieved, this value is the same as the ArchiveSHA256TreeHash value. This field is null for the following:     * Archive retrieval jobs that specify a range that is not tree-hash aligned     * Archival jobs that specify a range that is equal to the whole archive, when the job status is @InProgress@      * Inventory jobs     * Select jobs
gjdSHA256TreeHash :: Lens' GlacierJobDescription (Maybe Text)
gjdSHA256TreeHash = lens _gjdSHA256TreeHash (\s a -> s {_gjdSHA256TreeHash = a})

-- | The archive ID requested for a select job or archive retrieval. Otherwise, this field is null.
gjdArchiveId :: Lens' GlacierJobDescription (Maybe Text)
gjdArchiveId = lens _gjdArchiveId (\s a -> s {_gjdArchiveId = a})

-- | Contains the parameters used for a select.
gjdSelectParameters :: Lens' GlacierJobDescription (Maybe SelectParameters)
gjdSelectParameters = lens _gjdSelectParameters (\s a -> s {_gjdSelectParameters = a})

-- | An opaque string that identifies an Amazon S3 Glacier job.
gjdJobId :: Lens' GlacierJobDescription (Maybe Text)
gjdJobId = lens _gjdJobId (\s a -> s {_gjdJobId = a})

-- | Contains the job output location.
gjdJobOutputPath :: Lens' GlacierJobDescription (Maybe Text)
gjdJobOutputPath = lens _gjdJobOutputPath (\s a -> s {_gjdJobOutputPath = a})

-- | The retrieved byte range for archive retrieval jobs in the form /StartByteValue/ -/EndByteValue/ . If no range was specified in the archive retrieval, then the whole archive is retrieved. In this case, /StartByteValue/ equals 0 and /EndByteValue/ equals the size of the archive minus 1. For inventory retrieval or select jobs, this field is null.
gjdRetrievalByteRange :: Lens' GlacierJobDescription (Maybe Text)
gjdRetrievalByteRange = lens _gjdRetrievalByteRange (\s a -> s {_gjdRetrievalByteRange = a})

-- | Parameters used for range inventory retrieval.
gjdInventoryRetrievalParameters :: Lens' GlacierJobDescription (Maybe InventoryRetrievalJobDescription)
gjdInventoryRetrievalParameters = lens _gjdInventoryRetrievalParameters (\s a -> s {_gjdInventoryRetrievalParameters = a})

-- | The job type. This value is either @ArchiveRetrieval@ , @InventoryRetrieval@ , or @Select@ .
gjdAction :: Lens' GlacierJobDescription (Maybe ActionCode)
gjdAction = lens _gjdAction (\s a -> s {_gjdAction = a})

-- | The job description provided when initiating the job.
gjdJobDescription :: Lens' GlacierJobDescription (Maybe Text)
gjdJobDescription = lens _gjdJobDescription (\s a -> s {_gjdJobDescription = a})

-- | An Amazon SNS topic that receives notification.
gjdSNSTopic :: Lens' GlacierJobDescription (Maybe Text)
gjdSNSTopic = lens _gjdSNSTopic (\s a -> s {_gjdSNSTopic = a})

-- | A friendly message that describes the job status.
gjdStatusMessage :: Lens' GlacierJobDescription (Maybe Text)
gjdStatusMessage = lens _gjdStatusMessage (\s a -> s {_gjdStatusMessage = a})

-- | The Amazon Resource Name (ARN) of the vault from which an archive retrieval was requested.
gjdVaultARN :: Lens' GlacierJobDescription (Maybe Text)
gjdVaultARN = lens _gjdVaultARN (\s a -> s {_gjdVaultARN = a})

-- | Contains the location where the data from the select job is stored.
gjdOutputLocation :: Lens' GlacierJobDescription (Maybe OutputLocation)
gjdOutputLocation = lens _gjdOutputLocation (\s a -> s {_gjdOutputLocation = a})

-- | The tier to use for a select or an archive retrieval. Valid values are @Expedited@ , @Standard@ , or @Bulk@ . @Standard@ is the default.
gjdTier :: Lens' GlacierJobDescription (Maybe Text)
gjdTier = lens _gjdTier (\s a -> s {_gjdTier = a})

-- | The SHA256 tree hash of the entire archive for an archive retrieval. For inventory retrieval or select jobs, this field is null.
gjdArchiveSHA256TreeHash :: Lens' GlacierJobDescription (Maybe Text)
gjdArchiveSHA256TreeHash = lens _gjdArchiveSHA256TreeHash (\s a -> s {_gjdArchiveSHA256TreeHash = a})

-- | The UTC date when the job was created. This value is a string representation of ISO 8601 date format, for example @"2012-03-20T17:03:43.221Z"@ .
gjdCreationDate :: Lens' GlacierJobDescription (Maybe Text)
gjdCreationDate = lens _gjdCreationDate (\s a -> s {_gjdCreationDate = a})

-- | The job status. When a job is completed, you get the job's output using Get Job Output (GET output).
gjdCompleted :: Lens' GlacierJobDescription (Maybe Bool)
gjdCompleted = lens _gjdCompleted (\s a -> s {_gjdCompleted = a})

-- | The UTC time that the job request completed. While the job is in progress, the value is null.
gjdCompletionDate :: Lens' GlacierJobDescription (Maybe Text)
gjdCompletionDate = lens _gjdCompletionDate (\s a -> s {_gjdCompletionDate = a})

-- | For an inventory retrieval job, this value is the size in bytes of the inventory requested for download. For an archive retrieval or select job, this value is null.
gjdInventorySizeInBytes :: Lens' GlacierJobDescription (Maybe Integer)
gjdInventorySizeInBytes = lens _gjdInventorySizeInBytes (\s a -> s {_gjdInventorySizeInBytes = a})

-- | For an archive retrieval job, this value is the size in bytes of the archive being requested for download. For an inventory retrieval or select job, this value is null.
gjdArchiveSizeInBytes :: Lens' GlacierJobDescription (Maybe Integer)
gjdArchiveSizeInBytes = lens _gjdArchiveSizeInBytes (\s a -> s {_gjdArchiveSizeInBytes = a})

-- | The status code can be @InProgress@ , @Succeeded@ , or @Failed@ , and indicates the status of the job.
gjdStatusCode :: Lens' GlacierJobDescription (Maybe StatusCode)
gjdStatusCode = lens _gjdStatusCode (\s a -> s {_gjdStatusCode = a})

instance FromJSON GlacierJobDescription where
  parseJSON =
    withObject
      "GlacierJobDescription"
      ( \x ->
          GlacierJobDescription'
            <$> (x .:? "SHA256TreeHash")
            <*> (x .:? "ArchiveId")
            <*> (x .:? "SelectParameters")
            <*> (x .:? "JobId")
            <*> (x .:? "JobOutputPath")
            <*> (x .:? "RetrievalByteRange")
            <*> (x .:? "InventoryRetrievalParameters")
            <*> (x .:? "Action")
            <*> (x .:? "JobDescription")
            <*> (x .:? "SNSTopic")
            <*> (x .:? "StatusMessage")
            <*> (x .:? "VaultARN")
            <*> (x .:? "OutputLocation")
            <*> (x .:? "Tier")
            <*> (x .:? "ArchiveSHA256TreeHash")
            <*> (x .:? "CreationDate")
            <*> (x .:? "Completed")
            <*> (x .:? "CompletionDate")
            <*> (x .:? "InventorySizeInBytes")
            <*> (x .:? "ArchiveSizeInBytes")
            <*> (x .:? "StatusCode")
      )

instance Hashable GlacierJobDescription

instance NFData GlacierJobDescription
