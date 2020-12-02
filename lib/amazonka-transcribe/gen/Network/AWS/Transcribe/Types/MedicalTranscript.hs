{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.MedicalTranscript
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.MedicalTranscript where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Identifies the location of a medical transcript.
--
--
--
-- /See:/ 'medicalTranscript' smart constructor.
newtype MedicalTranscript = MedicalTranscript'
  { _mtTranscriptFileURI ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MedicalTranscript' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mtTranscriptFileURI' - The S3 object location of the medical transcript. Use this URI to access the medical transcript. This URI points to the S3 bucket you created to store the medical transcript.
medicalTranscript ::
  MedicalTranscript
medicalTranscript =
  MedicalTranscript' {_mtTranscriptFileURI = Nothing}

-- | The S3 object location of the medical transcript. Use this URI to access the medical transcript. This URI points to the S3 bucket you created to store the medical transcript.
mtTranscriptFileURI :: Lens' MedicalTranscript (Maybe Text)
mtTranscriptFileURI = lens _mtTranscriptFileURI (\s a -> s {_mtTranscriptFileURI = a})

instance FromJSON MedicalTranscript where
  parseJSON =
    withObject
      "MedicalTranscript"
      (\x -> MedicalTranscript' <$> (x .:? "TranscriptFileUri"))

instance Hashable MedicalTranscript

instance NFData MedicalTranscript
