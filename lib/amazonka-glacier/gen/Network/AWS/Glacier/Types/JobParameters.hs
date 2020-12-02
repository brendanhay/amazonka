{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.JobParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.JobParameters where

import Network.AWS.Glacier.Types.InventoryRetrievalJobInput
import Network.AWS.Glacier.Types.OutputLocation
import Network.AWS.Glacier.Types.SelectParameters
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides options for defining a job.
--
--
--
-- /See:/ 'jobParameters' smart constructor.
data JobParameters = JobParameters'
  { _jpArchiveId :: !(Maybe Text),
    _jpSelectParameters :: !(Maybe SelectParameters),
    _jpFormat :: !(Maybe Text),
    _jpRetrievalByteRange :: !(Maybe Text),
    _jpInventoryRetrievalParameters ::
      !(Maybe InventoryRetrievalJobInput),
    _jpSNSTopic :: !(Maybe Text),
    _jpOutputLocation :: !(Maybe OutputLocation),
    _jpTier :: !(Maybe Text),
    _jpType :: !(Maybe Text),
    _jpDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JobParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jpArchiveId' - The ID of the archive that you want to retrieve. This field is required only if @Type@ is set to @select@ or @archive-retrieval@ code>. An error occurs if you specify this request parameter for an inventory retrieval job request.
--
-- * 'jpSelectParameters' - Contains the parameters that define a job.
--
-- * 'jpFormat' - When initiating a job to retrieve a vault inventory, you can optionally add this parameter to your request to specify the output format. If you are initiating an inventory job and do not specify a Format field, JSON is the default format. Valid values are "CSV" and "JSON".
--
-- * 'jpRetrievalByteRange' - The byte range to retrieve for an archive retrieval. in the form "/StartByteValue/ -/EndByteValue/ " If not specified, the whole archive is retrieved. If specified, the byte range must be megabyte (1024*1024) aligned which means that /StartByteValue/ must be divisible by 1 MB and /EndByteValue/ plus 1 must be divisible by 1 MB or be the end of the archive specified as the archive byte size value minus 1. If RetrievalByteRange is not megabyte aligned, this operation returns a 400 response.  An error occurs if you specify this field for an inventory retrieval job request.
--
-- * 'jpInventoryRetrievalParameters' - Input parameters used for range inventory retrieval.
--
-- * 'jpSNSTopic' - The Amazon SNS topic ARN to which Amazon S3 Glacier sends a notification when the job is completed and the output is ready for you to download. The specified topic publishes the notification to its subscribers. The SNS topic must exist.
--
-- * 'jpOutputLocation' - Contains information about the location where the select job results are stored.
--
-- * 'jpTier' - The tier to use for a select or an archive retrieval job. Valid values are @Expedited@ , @Standard@ , or @Bulk@ . @Standard@ is the default.
--
-- * 'jpType' - The job type. You can initiate a job to perform a select query on an archive, retrieve an archive, or get an inventory of a vault. Valid values are "select", "archive-retrieval" and "inventory-retrieval".
--
-- * 'jpDescription' - The optional description for the job. The description must be less than or equal to 1,024 bytes. The allowable characters are 7-bit ASCII without control codes-specifically, ASCII values 32-126 decimal or 0x20-0x7E hexadecimal.
jobParameters ::
  JobParameters
jobParameters =
  JobParameters'
    { _jpArchiveId = Nothing,
      _jpSelectParameters = Nothing,
      _jpFormat = Nothing,
      _jpRetrievalByteRange = Nothing,
      _jpInventoryRetrievalParameters = Nothing,
      _jpSNSTopic = Nothing,
      _jpOutputLocation = Nothing,
      _jpTier = Nothing,
      _jpType = Nothing,
      _jpDescription = Nothing
    }

-- | The ID of the archive that you want to retrieve. This field is required only if @Type@ is set to @select@ or @archive-retrieval@ code>. An error occurs if you specify this request parameter for an inventory retrieval job request.
jpArchiveId :: Lens' JobParameters (Maybe Text)
jpArchiveId = lens _jpArchiveId (\s a -> s {_jpArchiveId = a})

-- | Contains the parameters that define a job.
jpSelectParameters :: Lens' JobParameters (Maybe SelectParameters)
jpSelectParameters = lens _jpSelectParameters (\s a -> s {_jpSelectParameters = a})

-- | When initiating a job to retrieve a vault inventory, you can optionally add this parameter to your request to specify the output format. If you are initiating an inventory job and do not specify a Format field, JSON is the default format. Valid values are "CSV" and "JSON".
jpFormat :: Lens' JobParameters (Maybe Text)
jpFormat = lens _jpFormat (\s a -> s {_jpFormat = a})

-- | The byte range to retrieve for an archive retrieval. in the form "/StartByteValue/ -/EndByteValue/ " If not specified, the whole archive is retrieved. If specified, the byte range must be megabyte (1024*1024) aligned which means that /StartByteValue/ must be divisible by 1 MB and /EndByteValue/ plus 1 must be divisible by 1 MB or be the end of the archive specified as the archive byte size value minus 1. If RetrievalByteRange is not megabyte aligned, this operation returns a 400 response.  An error occurs if you specify this field for an inventory retrieval job request.
jpRetrievalByteRange :: Lens' JobParameters (Maybe Text)
jpRetrievalByteRange = lens _jpRetrievalByteRange (\s a -> s {_jpRetrievalByteRange = a})

-- | Input parameters used for range inventory retrieval.
jpInventoryRetrievalParameters :: Lens' JobParameters (Maybe InventoryRetrievalJobInput)
jpInventoryRetrievalParameters = lens _jpInventoryRetrievalParameters (\s a -> s {_jpInventoryRetrievalParameters = a})

-- | The Amazon SNS topic ARN to which Amazon S3 Glacier sends a notification when the job is completed and the output is ready for you to download. The specified topic publishes the notification to its subscribers. The SNS topic must exist.
jpSNSTopic :: Lens' JobParameters (Maybe Text)
jpSNSTopic = lens _jpSNSTopic (\s a -> s {_jpSNSTopic = a})

-- | Contains information about the location where the select job results are stored.
jpOutputLocation :: Lens' JobParameters (Maybe OutputLocation)
jpOutputLocation = lens _jpOutputLocation (\s a -> s {_jpOutputLocation = a})

-- | The tier to use for a select or an archive retrieval job. Valid values are @Expedited@ , @Standard@ , or @Bulk@ . @Standard@ is the default.
jpTier :: Lens' JobParameters (Maybe Text)
jpTier = lens _jpTier (\s a -> s {_jpTier = a})

-- | The job type. You can initiate a job to perform a select query on an archive, retrieve an archive, or get an inventory of a vault. Valid values are "select", "archive-retrieval" and "inventory-retrieval".
jpType :: Lens' JobParameters (Maybe Text)
jpType = lens _jpType (\s a -> s {_jpType = a})

-- | The optional description for the job. The description must be less than or equal to 1,024 bytes. The allowable characters are 7-bit ASCII without control codes-specifically, ASCII values 32-126 decimal or 0x20-0x7E hexadecimal.
jpDescription :: Lens' JobParameters (Maybe Text)
jpDescription = lens _jpDescription (\s a -> s {_jpDescription = a})

instance Hashable JobParameters

instance NFData JobParameters

instance ToJSON JobParameters where
  toJSON JobParameters' {..} =
    object
      ( catMaybes
          [ ("ArchiveId" .=) <$> _jpArchiveId,
            ("SelectParameters" .=) <$> _jpSelectParameters,
            ("Format" .=) <$> _jpFormat,
            ("RetrievalByteRange" .=) <$> _jpRetrievalByteRange,
            ("InventoryRetrievalParameters" .=)
              <$> _jpInventoryRetrievalParameters,
            ("SNSTopic" .=) <$> _jpSNSTopic,
            ("OutputLocation" .=) <$> _jpOutputLocation,
            ("Tier" .=) <$> _jpTier,
            ("Type" .=) <$> _jpType,
            ("Description" .=) <$> _jpDescription
          ]
      )
