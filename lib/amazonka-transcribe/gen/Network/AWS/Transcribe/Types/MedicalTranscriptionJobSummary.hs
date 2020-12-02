{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.MedicalTranscriptionJobSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.MedicalTranscriptionJobSummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Transcribe.Types.LanguageCode
import Network.AWS.Transcribe.Types.OutputLocationType
import Network.AWS.Transcribe.Types.Specialty
import Network.AWS.Transcribe.Types.TranscriptionJobStatus
import Network.AWS.Transcribe.Types.Type

-- | Provides summary information about a transcription job.
--
--
--
-- /See:/ 'medicalTranscriptionJobSummary' smart constructor.
data MedicalTranscriptionJobSummary = MedicalTranscriptionJobSummary'
  { _mtjsCreationTime ::
      !(Maybe POSIX),
    _mtjsSpecialty ::
      !(Maybe Specialty),
    _mtjsFailureReason ::
      !(Maybe Text),
    _mtjsLanguageCode ::
      !(Maybe LanguageCode),
    _mtjsOutputLocationType ::
      !(Maybe OutputLocationType),
    _mtjsStartTime ::
      !(Maybe POSIX),
    _mtjsCompletionTime ::
      !(Maybe POSIX),
    _mtjsMedicalTranscriptionJobName ::
      !(Maybe Text),
    _mtjsTranscriptionJobStatus ::
      !( Maybe
           TranscriptionJobStatus
       ),
    _mtjsType :: !(Maybe Type)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MedicalTranscriptionJobSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mtjsCreationTime' - A timestamp that shows when the medical transcription job was created.
--
-- * 'mtjsSpecialty' - The medical specialty of the transcription job. @Primary care@ is the only valid value.
--
-- * 'mtjsFailureReason' - If the @TranscriptionJobStatus@ field is @FAILED@ , a description of the error.
--
-- * 'mtjsLanguageCode' - The language of the transcript in the source audio file.
--
-- * 'mtjsOutputLocationType' - Indicates the location of the transcription job's output. The @CUSTOMER_BUCKET@ is the S3 location provided in the @OutputBucketName@ field when the
--
-- * 'mtjsStartTime' - A timestamp that shows when the job began processing.
--
-- * 'mtjsCompletionTime' - A timestamp that shows when the job was completed.
--
-- * 'mtjsMedicalTranscriptionJobName' - The name of a medical transcription job.
--
-- * 'mtjsTranscriptionJobStatus' - The status of the medical transcription job.
--
-- * 'mtjsType' - The speech of the clinician in the input audio.
medicalTranscriptionJobSummary ::
  MedicalTranscriptionJobSummary
medicalTranscriptionJobSummary =
  MedicalTranscriptionJobSummary'
    { _mtjsCreationTime = Nothing,
      _mtjsSpecialty = Nothing,
      _mtjsFailureReason = Nothing,
      _mtjsLanguageCode = Nothing,
      _mtjsOutputLocationType = Nothing,
      _mtjsStartTime = Nothing,
      _mtjsCompletionTime = Nothing,
      _mtjsMedicalTranscriptionJobName = Nothing,
      _mtjsTranscriptionJobStatus = Nothing,
      _mtjsType = Nothing
    }

-- | A timestamp that shows when the medical transcription job was created.
mtjsCreationTime :: Lens' MedicalTranscriptionJobSummary (Maybe UTCTime)
mtjsCreationTime = lens _mtjsCreationTime (\s a -> s {_mtjsCreationTime = a}) . mapping _Time

-- | The medical specialty of the transcription job. @Primary care@ is the only valid value.
mtjsSpecialty :: Lens' MedicalTranscriptionJobSummary (Maybe Specialty)
mtjsSpecialty = lens _mtjsSpecialty (\s a -> s {_mtjsSpecialty = a})

-- | If the @TranscriptionJobStatus@ field is @FAILED@ , a description of the error.
mtjsFailureReason :: Lens' MedicalTranscriptionJobSummary (Maybe Text)
mtjsFailureReason = lens _mtjsFailureReason (\s a -> s {_mtjsFailureReason = a})

-- | The language of the transcript in the source audio file.
mtjsLanguageCode :: Lens' MedicalTranscriptionJobSummary (Maybe LanguageCode)
mtjsLanguageCode = lens _mtjsLanguageCode (\s a -> s {_mtjsLanguageCode = a})

-- | Indicates the location of the transcription job's output. The @CUSTOMER_BUCKET@ is the S3 location provided in the @OutputBucketName@ field when the
mtjsOutputLocationType :: Lens' MedicalTranscriptionJobSummary (Maybe OutputLocationType)
mtjsOutputLocationType = lens _mtjsOutputLocationType (\s a -> s {_mtjsOutputLocationType = a})

-- | A timestamp that shows when the job began processing.
mtjsStartTime :: Lens' MedicalTranscriptionJobSummary (Maybe UTCTime)
mtjsStartTime = lens _mtjsStartTime (\s a -> s {_mtjsStartTime = a}) . mapping _Time

-- | A timestamp that shows when the job was completed.
mtjsCompletionTime :: Lens' MedicalTranscriptionJobSummary (Maybe UTCTime)
mtjsCompletionTime = lens _mtjsCompletionTime (\s a -> s {_mtjsCompletionTime = a}) . mapping _Time

-- | The name of a medical transcription job.
mtjsMedicalTranscriptionJobName :: Lens' MedicalTranscriptionJobSummary (Maybe Text)
mtjsMedicalTranscriptionJobName = lens _mtjsMedicalTranscriptionJobName (\s a -> s {_mtjsMedicalTranscriptionJobName = a})

-- | The status of the medical transcription job.
mtjsTranscriptionJobStatus :: Lens' MedicalTranscriptionJobSummary (Maybe TranscriptionJobStatus)
mtjsTranscriptionJobStatus = lens _mtjsTranscriptionJobStatus (\s a -> s {_mtjsTranscriptionJobStatus = a})

-- | The speech of the clinician in the input audio.
mtjsType :: Lens' MedicalTranscriptionJobSummary (Maybe Type)
mtjsType = lens _mtjsType (\s a -> s {_mtjsType = a})

instance FromJSON MedicalTranscriptionJobSummary where
  parseJSON =
    withObject
      "MedicalTranscriptionJobSummary"
      ( \x ->
          MedicalTranscriptionJobSummary'
            <$> (x .:? "CreationTime")
            <*> (x .:? "Specialty")
            <*> (x .:? "FailureReason")
            <*> (x .:? "LanguageCode")
            <*> (x .:? "OutputLocationType")
            <*> (x .:? "StartTime")
            <*> (x .:? "CompletionTime")
            <*> (x .:? "MedicalTranscriptionJobName")
            <*> (x .:? "TranscriptionJobStatus")
            <*> (x .:? "Type")
      )

instance Hashable MedicalTranscriptionJobSummary

instance NFData MedicalTranscriptionJobSummary
