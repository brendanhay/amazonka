{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.ContentRedaction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.ContentRedaction where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Transcribe.Types.RedactionOutput
import Network.AWS.Transcribe.Types.RedactionType

-- | Settings for content redaction within a transcription job.
--
-- /See:/ 'newContentRedaction' smart constructor.
data ContentRedaction = ContentRedaction'
  { -- | Request parameter that defines the entities to be redacted. The only
    -- accepted value is @PII@.
    redactionType :: RedactionType,
    -- | The output transcript file stored in either the default S3 bucket or in
    -- a bucket you specify.
    --
    -- When you choose @redacted@ Amazon Transcribe outputs only the redacted
    -- transcript.
    --
    -- When you choose @redacted_and_unredacted@ Amazon Transcribe outputs both
    -- the redacted and unredacted transcripts.
    redactionOutput :: RedactionOutput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ContentRedaction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'redactionType', 'contentRedaction_redactionType' - Request parameter that defines the entities to be redacted. The only
-- accepted value is @PII@.
--
-- 'redactionOutput', 'contentRedaction_redactionOutput' - The output transcript file stored in either the default S3 bucket or in
-- a bucket you specify.
--
-- When you choose @redacted@ Amazon Transcribe outputs only the redacted
-- transcript.
--
-- When you choose @redacted_and_unredacted@ Amazon Transcribe outputs both
-- the redacted and unredacted transcripts.
newContentRedaction ::
  -- | 'redactionType'
  RedactionType ->
  -- | 'redactionOutput'
  RedactionOutput ->
  ContentRedaction
newContentRedaction pRedactionType_ pRedactionOutput_ =
  ContentRedaction'
    { redactionType = pRedactionType_,
      redactionOutput = pRedactionOutput_
    }

-- | Request parameter that defines the entities to be redacted. The only
-- accepted value is @PII@.
contentRedaction_redactionType :: Lens.Lens' ContentRedaction RedactionType
contentRedaction_redactionType = Lens.lens (\ContentRedaction' {redactionType} -> redactionType) (\s@ContentRedaction' {} a -> s {redactionType = a} :: ContentRedaction)

-- | The output transcript file stored in either the default S3 bucket or in
-- a bucket you specify.
--
-- When you choose @redacted@ Amazon Transcribe outputs only the redacted
-- transcript.
--
-- When you choose @redacted_and_unredacted@ Amazon Transcribe outputs both
-- the redacted and unredacted transcripts.
contentRedaction_redactionOutput :: Lens.Lens' ContentRedaction RedactionOutput
contentRedaction_redactionOutput = Lens.lens (\ContentRedaction' {redactionOutput} -> redactionOutput) (\s@ContentRedaction' {} a -> s {redactionOutput = a} :: ContentRedaction)

instance Prelude.FromJSON ContentRedaction where
  parseJSON =
    Prelude.withObject
      "ContentRedaction"
      ( \x ->
          ContentRedaction'
            Prelude.<$> (x Prelude..: "RedactionType")
            Prelude.<*> (x Prelude..: "RedactionOutput")
      )

instance Prelude.Hashable ContentRedaction

instance Prelude.NFData ContentRedaction

instance Prelude.ToJSON ContentRedaction where
  toJSON ContentRedaction' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("RedactionType" Prelude..= redactionType),
            Prelude.Just
              ("RedactionOutput" Prelude..= redactionOutput)
          ]
      )
