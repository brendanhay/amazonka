{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.ContentRedaction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.ContentRedaction where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Transcribe.Types.RedactionOutput
import Network.AWS.Transcribe.Types.RedactionType

-- | Settings for content redaction within a transcription job.
--
--
--
-- /See:/ 'contentRedaction' smart constructor.
data ContentRedaction = ContentRedaction'
  { _crRedactionType ::
      !RedactionType,
    _crRedactionOutput :: !RedactionOutput
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ContentRedaction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crRedactionType' - Request parameter that defines the entities to be redacted. The only accepted value is @PII@ .
--
-- * 'crRedactionOutput' - The output transcript file stored in either the default S3 bucket or in a bucket you specify. When you choose @redacted@ Amazon Transcribe outputs only the redacted transcript. When you choose @redacted_and_unredacted@ Amazon Transcribe outputs both the redacted and unredacted transcripts.
contentRedaction ::
  -- | 'crRedactionType'
  RedactionType ->
  -- | 'crRedactionOutput'
  RedactionOutput ->
  ContentRedaction
contentRedaction pRedactionType_ pRedactionOutput_ =
  ContentRedaction'
    { _crRedactionType = pRedactionType_,
      _crRedactionOutput = pRedactionOutput_
    }

-- | Request parameter that defines the entities to be redacted. The only accepted value is @PII@ .
crRedactionType :: Lens' ContentRedaction RedactionType
crRedactionType = lens _crRedactionType (\s a -> s {_crRedactionType = a})

-- | The output transcript file stored in either the default S3 bucket or in a bucket you specify. When you choose @redacted@ Amazon Transcribe outputs only the redacted transcript. When you choose @redacted_and_unredacted@ Amazon Transcribe outputs both the redacted and unredacted transcripts.
crRedactionOutput :: Lens' ContentRedaction RedactionOutput
crRedactionOutput = lens _crRedactionOutput (\s a -> s {_crRedactionOutput = a})

instance FromJSON ContentRedaction where
  parseJSON =
    withObject
      "ContentRedaction"
      ( \x ->
          ContentRedaction'
            <$> (x .: "RedactionType") <*> (x .: "RedactionOutput")
      )

instance Hashable ContentRedaction

instance NFData ContentRedaction

instance ToJSON ContentRedaction where
  toJSON ContentRedaction' {..} =
    object
      ( catMaybes
          [ Just ("RedactionType" .= _crRedactionType),
            Just ("RedactionOutput" .= _crRedactionOutput)
          ]
      )
