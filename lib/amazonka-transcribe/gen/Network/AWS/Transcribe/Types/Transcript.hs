{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.Transcript
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.Transcript where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Identifies the location of a transcription.
--
--
--
-- /See:/ 'transcript' smart constructor.
data Transcript = Transcript'
  { _tRedactedTranscriptFileURI ::
      !(Maybe Text),
    _tTranscriptFileURI :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Transcript' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tRedactedTranscriptFileURI' - The S3 object location of the redacted transcript. Use this URI to access the redacted transcript. If you specified an S3 bucket in the @OutputBucketName@ field when you created the job, this is the URI of that bucket. If you chose to store the transcript in Amazon Transcribe, this is a shareable URL that provides secure access to that location.
--
-- * 'tTranscriptFileURI' - The S3 object location of the transcript. Use this URI to access the transcript. If you specified an S3 bucket in the @OutputBucketName@ field when you created the job, this is the URI of that bucket. If you chose to store the transcript in Amazon Transcribe, this is a shareable URL that provides secure access to that location.
transcript ::
  Transcript
transcript =
  Transcript'
    { _tRedactedTranscriptFileURI = Nothing,
      _tTranscriptFileURI = Nothing
    }

-- | The S3 object location of the redacted transcript. Use this URI to access the redacted transcript. If you specified an S3 bucket in the @OutputBucketName@ field when you created the job, this is the URI of that bucket. If you chose to store the transcript in Amazon Transcribe, this is a shareable URL that provides secure access to that location.
tRedactedTranscriptFileURI :: Lens' Transcript (Maybe Text)
tRedactedTranscriptFileURI = lens _tRedactedTranscriptFileURI (\s a -> s {_tRedactedTranscriptFileURI = a})

-- | The S3 object location of the transcript. Use this URI to access the transcript. If you specified an S3 bucket in the @OutputBucketName@ field when you created the job, this is the URI of that bucket. If you chose to store the transcript in Amazon Transcribe, this is a shareable URL that provides secure access to that location.
tTranscriptFileURI :: Lens' Transcript (Maybe Text)
tTranscriptFileURI = lens _tTranscriptFileURI (\s a -> s {_tTranscriptFileURI = a})

instance FromJSON Transcript where
  parseJSON =
    withObject
      "Transcript"
      ( \x ->
          Transcript'
            <$> (x .:? "RedactedTranscriptFileUri")
            <*> (x .:? "TranscriptFileUri")
      )

instance Hashable Transcript

instance NFData Transcript
