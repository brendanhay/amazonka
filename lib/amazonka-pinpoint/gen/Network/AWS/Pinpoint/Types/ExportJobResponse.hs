{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ExportJobResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ExportJobResponse where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.ExportJobResource
import Network.AWS.Pinpoint.Types.JobStatus
import Network.AWS.Prelude

-- | Provides information about the status and settings of a job that exports endpoint definitions to a file. The file can be added directly to an Amazon Simple Storage Service (Amazon S3) bucket by using the Amazon Pinpoint API or downloaded directly to a computer by using the Amazon Pinpoint console.
--
--
--
-- /See:/ 'exportJobResponse' smart constructor.
data ExportJobResponse = ExportJobResponse'
  { _ejCompletedPieces ::
      !(Maybe Int),
    _ejFailedPieces :: !(Maybe Int),
    _ejTotalProcessed :: !(Maybe Int),
    _ejFailures :: !(Maybe [Text]),
    _ejTotalPieces :: !(Maybe Int),
    _ejCompletionDate :: !(Maybe Text),
    _ejTotalFailures :: !(Maybe Int),
    _ejJobStatus :: !JobStatus,
    _ejCreationDate :: !Text,
    _ejType :: !Text,
    _ejDefinition :: !ExportJobResource,
    _ejId :: !Text,
    _ejApplicationId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExportJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ejCompletedPieces' - The number of pieces that were processed successfully (completed) by the export job, as of the time of the request.
--
-- * 'ejFailedPieces' - The number of pieces that weren't processed successfully (failed) by the export job, as of the time of the request.
--
-- * 'ejTotalProcessed' - The total number of endpoint definitions that were processed by the export job.
--
-- * 'ejFailures' - An array of entries, one for each of the first 100 entries that weren't processed successfully (failed) by the export job, if any.
--
-- * 'ejTotalPieces' - The total number of pieces that must be processed to complete the export job. Each piece consists of an approximately equal portion of the endpoint definitions that are part of the export job.
--
-- * 'ejCompletionDate' - The date, in ISO 8601 format, when the export job was completed.
--
-- * 'ejTotalFailures' - The total number of endpoint definitions that weren't processed successfully (failed) by the export job, typically because an error, such as a syntax error, occurred.
--
-- * 'ejJobStatus' - The status of the export job. The job status is FAILED if Amazon Pinpoint wasn't able to process one or more pieces in the job.
--
-- * 'ejCreationDate' - The date, in ISO 8601 format, when the export job was created.
--
-- * 'ejType' - The job type. This value is EXPORT for export jobs.
--
-- * 'ejDefinition' - The resource settings that apply to the export job.
--
-- * 'ejId' - The unique identifier for the export job.
--
-- * 'ejApplicationId' - The unique identifier for the application that's associated with the export job.
exportJobResponse ::
  -- | 'ejJobStatus'
  JobStatus ->
  -- | 'ejCreationDate'
  Text ->
  -- | 'ejType'
  Text ->
  -- | 'ejDefinition'
  ExportJobResource ->
  -- | 'ejId'
  Text ->
  -- | 'ejApplicationId'
  Text ->
  ExportJobResponse
exportJobResponse
  pJobStatus_
  pCreationDate_
  pType_
  pDefinition_
  pId_
  pApplicationId_ =
    ExportJobResponse'
      { _ejCompletedPieces = Nothing,
        _ejFailedPieces = Nothing,
        _ejTotalProcessed = Nothing,
        _ejFailures = Nothing,
        _ejTotalPieces = Nothing,
        _ejCompletionDate = Nothing,
        _ejTotalFailures = Nothing,
        _ejJobStatus = pJobStatus_,
        _ejCreationDate = pCreationDate_,
        _ejType = pType_,
        _ejDefinition = pDefinition_,
        _ejId = pId_,
        _ejApplicationId = pApplicationId_
      }

-- | The number of pieces that were processed successfully (completed) by the export job, as of the time of the request.
ejCompletedPieces :: Lens' ExportJobResponse (Maybe Int)
ejCompletedPieces = lens _ejCompletedPieces (\s a -> s {_ejCompletedPieces = a})

-- | The number of pieces that weren't processed successfully (failed) by the export job, as of the time of the request.
ejFailedPieces :: Lens' ExportJobResponse (Maybe Int)
ejFailedPieces = lens _ejFailedPieces (\s a -> s {_ejFailedPieces = a})

-- | The total number of endpoint definitions that were processed by the export job.
ejTotalProcessed :: Lens' ExportJobResponse (Maybe Int)
ejTotalProcessed = lens _ejTotalProcessed (\s a -> s {_ejTotalProcessed = a})

-- | An array of entries, one for each of the first 100 entries that weren't processed successfully (failed) by the export job, if any.
ejFailures :: Lens' ExportJobResponse [Text]
ejFailures = lens _ejFailures (\s a -> s {_ejFailures = a}) . _Default . _Coerce

-- | The total number of pieces that must be processed to complete the export job. Each piece consists of an approximately equal portion of the endpoint definitions that are part of the export job.
ejTotalPieces :: Lens' ExportJobResponse (Maybe Int)
ejTotalPieces = lens _ejTotalPieces (\s a -> s {_ejTotalPieces = a})

-- | The date, in ISO 8601 format, when the export job was completed.
ejCompletionDate :: Lens' ExportJobResponse (Maybe Text)
ejCompletionDate = lens _ejCompletionDate (\s a -> s {_ejCompletionDate = a})

-- | The total number of endpoint definitions that weren't processed successfully (failed) by the export job, typically because an error, such as a syntax error, occurred.
ejTotalFailures :: Lens' ExportJobResponse (Maybe Int)
ejTotalFailures = lens _ejTotalFailures (\s a -> s {_ejTotalFailures = a})

-- | The status of the export job. The job status is FAILED if Amazon Pinpoint wasn't able to process one or more pieces in the job.
ejJobStatus :: Lens' ExportJobResponse JobStatus
ejJobStatus = lens _ejJobStatus (\s a -> s {_ejJobStatus = a})

-- | The date, in ISO 8601 format, when the export job was created.
ejCreationDate :: Lens' ExportJobResponse Text
ejCreationDate = lens _ejCreationDate (\s a -> s {_ejCreationDate = a})

-- | The job type. This value is EXPORT for export jobs.
ejType :: Lens' ExportJobResponse Text
ejType = lens _ejType (\s a -> s {_ejType = a})

-- | The resource settings that apply to the export job.
ejDefinition :: Lens' ExportJobResponse ExportJobResource
ejDefinition = lens _ejDefinition (\s a -> s {_ejDefinition = a})

-- | The unique identifier for the export job.
ejId :: Lens' ExportJobResponse Text
ejId = lens _ejId (\s a -> s {_ejId = a})

-- | The unique identifier for the application that's associated with the export job.
ejApplicationId :: Lens' ExportJobResponse Text
ejApplicationId = lens _ejApplicationId (\s a -> s {_ejApplicationId = a})

instance FromJSON ExportJobResponse where
  parseJSON =
    withObject
      "ExportJobResponse"
      ( \x ->
          ExportJobResponse'
            <$> (x .:? "CompletedPieces")
            <*> (x .:? "FailedPieces")
            <*> (x .:? "TotalProcessed")
            <*> (x .:? "Failures" .!= mempty)
            <*> (x .:? "TotalPieces")
            <*> (x .:? "CompletionDate")
            <*> (x .:? "TotalFailures")
            <*> (x .: "JobStatus")
            <*> (x .: "CreationDate")
            <*> (x .: "Type")
            <*> (x .: "Definition")
            <*> (x .: "Id")
            <*> (x .: "ApplicationId")
      )

instance Hashable ExportJobResponse

instance NFData ExportJobResponse
