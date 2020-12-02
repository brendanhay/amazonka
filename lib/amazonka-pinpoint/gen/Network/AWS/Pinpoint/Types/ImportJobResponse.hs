{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ImportJobResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ImportJobResponse where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.ImportJobResource
import Network.AWS.Pinpoint.Types.JobStatus
import Network.AWS.Prelude

-- | Provides information about the status and settings of a job that imports endpoint definitions from one or more files. The files can be stored in an Amazon Simple Storage Service (Amazon S3) bucket or uploaded directly from a computer by using the Amazon Pinpoint console.
--
--
--
-- /See:/ 'importJobResponse' smart constructor.
data ImportJobResponse = ImportJobResponse'
  { _ijCompletedPieces ::
      !(Maybe Int),
    _ijFailedPieces :: !(Maybe Int),
    _ijTotalProcessed :: !(Maybe Int),
    _ijFailures :: !(Maybe [Text]),
    _ijTotalPieces :: !(Maybe Int),
    _ijCompletionDate :: !(Maybe Text),
    _ijTotalFailures :: !(Maybe Int),
    _ijJobStatus :: !JobStatus,
    _ijCreationDate :: !Text,
    _ijType :: !Text,
    _ijDefinition :: !ImportJobResource,
    _ijId :: !Text,
    _ijApplicationId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImportJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ijCompletedPieces' - The number of pieces that were processed successfully (completed) by the import job, as of the time of the request.
--
-- * 'ijFailedPieces' - The number of pieces that weren't processed successfully (failed) by the import job, as of the time of the request.
--
-- * 'ijTotalProcessed' - The total number of endpoint definitions that were processed by the import job.
--
-- * 'ijFailures' - An array of entries, one for each of the first 100 entries that weren't processed successfully (failed) by the import job, if any.
--
-- * 'ijTotalPieces' - The total number of pieces that must be processed to complete the import job. Each piece consists of an approximately equal portion of the endpoint definitions that are part of the import job.
--
-- * 'ijCompletionDate' - The date, in ISO 8601 format, when the import job was completed.
--
-- * 'ijTotalFailures' - The total number of endpoint definitions that weren't processed successfully (failed) by the import job, typically because an error, such as a syntax error, occurred.
--
-- * 'ijJobStatus' - The status of the import job. The job status is FAILED if Amazon Pinpoint wasn't able to process one or more pieces in the job.
--
-- * 'ijCreationDate' - The date, in ISO 8601 format, when the import job was created.
--
-- * 'ijType' - The job type. This value is IMPORT for import jobs.
--
-- * 'ijDefinition' - The resource settings that apply to the import job.
--
-- * 'ijId' - The unique identifier for the import job.
--
-- * 'ijApplicationId' - The unique identifier for the application that's associated with the import job.
importJobResponse ::
  -- | 'ijJobStatus'
  JobStatus ->
  -- | 'ijCreationDate'
  Text ->
  -- | 'ijType'
  Text ->
  -- | 'ijDefinition'
  ImportJobResource ->
  -- | 'ijId'
  Text ->
  -- | 'ijApplicationId'
  Text ->
  ImportJobResponse
importJobResponse
  pJobStatus_
  pCreationDate_
  pType_
  pDefinition_
  pId_
  pApplicationId_ =
    ImportJobResponse'
      { _ijCompletedPieces = Nothing,
        _ijFailedPieces = Nothing,
        _ijTotalProcessed = Nothing,
        _ijFailures = Nothing,
        _ijTotalPieces = Nothing,
        _ijCompletionDate = Nothing,
        _ijTotalFailures = Nothing,
        _ijJobStatus = pJobStatus_,
        _ijCreationDate = pCreationDate_,
        _ijType = pType_,
        _ijDefinition = pDefinition_,
        _ijId = pId_,
        _ijApplicationId = pApplicationId_
      }

-- | The number of pieces that were processed successfully (completed) by the import job, as of the time of the request.
ijCompletedPieces :: Lens' ImportJobResponse (Maybe Int)
ijCompletedPieces = lens _ijCompletedPieces (\s a -> s {_ijCompletedPieces = a})

-- | The number of pieces that weren't processed successfully (failed) by the import job, as of the time of the request.
ijFailedPieces :: Lens' ImportJobResponse (Maybe Int)
ijFailedPieces = lens _ijFailedPieces (\s a -> s {_ijFailedPieces = a})

-- | The total number of endpoint definitions that were processed by the import job.
ijTotalProcessed :: Lens' ImportJobResponse (Maybe Int)
ijTotalProcessed = lens _ijTotalProcessed (\s a -> s {_ijTotalProcessed = a})

-- | An array of entries, one for each of the first 100 entries that weren't processed successfully (failed) by the import job, if any.
ijFailures :: Lens' ImportJobResponse [Text]
ijFailures = lens _ijFailures (\s a -> s {_ijFailures = a}) . _Default . _Coerce

-- | The total number of pieces that must be processed to complete the import job. Each piece consists of an approximately equal portion of the endpoint definitions that are part of the import job.
ijTotalPieces :: Lens' ImportJobResponse (Maybe Int)
ijTotalPieces = lens _ijTotalPieces (\s a -> s {_ijTotalPieces = a})

-- | The date, in ISO 8601 format, when the import job was completed.
ijCompletionDate :: Lens' ImportJobResponse (Maybe Text)
ijCompletionDate = lens _ijCompletionDate (\s a -> s {_ijCompletionDate = a})

-- | The total number of endpoint definitions that weren't processed successfully (failed) by the import job, typically because an error, such as a syntax error, occurred.
ijTotalFailures :: Lens' ImportJobResponse (Maybe Int)
ijTotalFailures = lens _ijTotalFailures (\s a -> s {_ijTotalFailures = a})

-- | The status of the import job. The job status is FAILED if Amazon Pinpoint wasn't able to process one or more pieces in the job.
ijJobStatus :: Lens' ImportJobResponse JobStatus
ijJobStatus = lens _ijJobStatus (\s a -> s {_ijJobStatus = a})

-- | The date, in ISO 8601 format, when the import job was created.
ijCreationDate :: Lens' ImportJobResponse Text
ijCreationDate = lens _ijCreationDate (\s a -> s {_ijCreationDate = a})

-- | The job type. This value is IMPORT for import jobs.
ijType :: Lens' ImportJobResponse Text
ijType = lens _ijType (\s a -> s {_ijType = a})

-- | The resource settings that apply to the import job.
ijDefinition :: Lens' ImportJobResponse ImportJobResource
ijDefinition = lens _ijDefinition (\s a -> s {_ijDefinition = a})

-- | The unique identifier for the import job.
ijId :: Lens' ImportJobResponse Text
ijId = lens _ijId (\s a -> s {_ijId = a})

-- | The unique identifier for the application that's associated with the import job.
ijApplicationId :: Lens' ImportJobResponse Text
ijApplicationId = lens _ijApplicationId (\s a -> s {_ijApplicationId = a})

instance FromJSON ImportJobResponse where
  parseJSON =
    withObject
      "ImportJobResponse"
      ( \x ->
          ImportJobResponse'
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

instance Hashable ImportJobResponse

instance NFData ImportJobResponse
