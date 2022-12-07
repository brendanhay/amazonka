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
-- Module      : Amazonka.Transcribe.Types.ContentRedaction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.ContentRedaction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transcribe.Types.PiiEntityType
import Amazonka.Transcribe.Types.RedactionOutput
import Amazonka.Transcribe.Types.RedactionType

-- | Allows you to redact or flag specified personally identifiable
-- information (PII) in your transcript. If you use @ContentRedaction@, you
-- must also include the sub-parameters: @PiiEntityTypes@,
-- @RedactionOutput@, and @RedactionType@.
--
-- /See:/ 'newContentRedaction' smart constructor.
data ContentRedaction = ContentRedaction'
  { -- | Specify which types of personally identifiable information (PII) you
    -- want to redact in your transcript. You can include as many types as
    -- you\'d like, or you can select @ALL@.
    piiEntityTypes :: Prelude.Maybe [PiiEntityType],
    -- | Specify the category of information you want to redact; @PII@
    -- (personally identifiable information) is the only valid value. You can
    -- use @PiiEntityTypes@ to choose which types of PII you want to redact.
    redactionType :: RedactionType,
    -- | Specify if you want only a redacted transcript, or if you want a
    -- redacted and an unredacted transcript.
    --
    -- When you choose @redacted@ Amazon Transcribe creates only a redacted
    -- transcript.
    --
    -- When you choose @redacted_and_unredacted@ Amazon Transcribe creates a
    -- redacted and an unredacted transcript (as two separate files).
    redactionOutput :: RedactionOutput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContentRedaction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'piiEntityTypes', 'contentRedaction_piiEntityTypes' - Specify which types of personally identifiable information (PII) you
-- want to redact in your transcript. You can include as many types as
-- you\'d like, or you can select @ALL@.
--
-- 'redactionType', 'contentRedaction_redactionType' - Specify the category of information you want to redact; @PII@
-- (personally identifiable information) is the only valid value. You can
-- use @PiiEntityTypes@ to choose which types of PII you want to redact.
--
-- 'redactionOutput', 'contentRedaction_redactionOutput' - Specify if you want only a redacted transcript, or if you want a
-- redacted and an unredacted transcript.
--
-- When you choose @redacted@ Amazon Transcribe creates only a redacted
-- transcript.
--
-- When you choose @redacted_and_unredacted@ Amazon Transcribe creates a
-- redacted and an unredacted transcript (as two separate files).
newContentRedaction ::
  -- | 'redactionType'
  RedactionType ->
  -- | 'redactionOutput'
  RedactionOutput ->
  ContentRedaction
newContentRedaction pRedactionType_ pRedactionOutput_ =
  ContentRedaction'
    { piiEntityTypes = Prelude.Nothing,
      redactionType = pRedactionType_,
      redactionOutput = pRedactionOutput_
    }

-- | Specify which types of personally identifiable information (PII) you
-- want to redact in your transcript. You can include as many types as
-- you\'d like, or you can select @ALL@.
contentRedaction_piiEntityTypes :: Lens.Lens' ContentRedaction (Prelude.Maybe [PiiEntityType])
contentRedaction_piiEntityTypes = Lens.lens (\ContentRedaction' {piiEntityTypes} -> piiEntityTypes) (\s@ContentRedaction' {} a -> s {piiEntityTypes = a} :: ContentRedaction) Prelude.. Lens.mapping Lens.coerced

-- | Specify the category of information you want to redact; @PII@
-- (personally identifiable information) is the only valid value. You can
-- use @PiiEntityTypes@ to choose which types of PII you want to redact.
contentRedaction_redactionType :: Lens.Lens' ContentRedaction RedactionType
contentRedaction_redactionType = Lens.lens (\ContentRedaction' {redactionType} -> redactionType) (\s@ContentRedaction' {} a -> s {redactionType = a} :: ContentRedaction)

-- | Specify if you want only a redacted transcript, or if you want a
-- redacted and an unredacted transcript.
--
-- When you choose @redacted@ Amazon Transcribe creates only a redacted
-- transcript.
--
-- When you choose @redacted_and_unredacted@ Amazon Transcribe creates a
-- redacted and an unredacted transcript (as two separate files).
contentRedaction_redactionOutput :: Lens.Lens' ContentRedaction RedactionOutput
contentRedaction_redactionOutput = Lens.lens (\ContentRedaction' {redactionOutput} -> redactionOutput) (\s@ContentRedaction' {} a -> s {redactionOutput = a} :: ContentRedaction)

instance Data.FromJSON ContentRedaction where
  parseJSON =
    Data.withObject
      "ContentRedaction"
      ( \x ->
          ContentRedaction'
            Prelude.<$> (x Data..:? "PiiEntityTypes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "RedactionType")
            Prelude.<*> (x Data..: "RedactionOutput")
      )

instance Prelude.Hashable ContentRedaction where
  hashWithSalt _salt ContentRedaction' {..} =
    _salt `Prelude.hashWithSalt` piiEntityTypes
      `Prelude.hashWithSalt` redactionType
      `Prelude.hashWithSalt` redactionOutput

instance Prelude.NFData ContentRedaction where
  rnf ContentRedaction' {..} =
    Prelude.rnf piiEntityTypes
      `Prelude.seq` Prelude.rnf redactionType
      `Prelude.seq` Prelude.rnf redactionOutput

instance Data.ToJSON ContentRedaction where
  toJSON ContentRedaction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PiiEntityTypes" Data..=)
              Prelude.<$> piiEntityTypes,
            Prelude.Just ("RedactionType" Data..= redactionType),
            Prelude.Just
              ("RedactionOutput" Data..= redactionOutput)
          ]
      )
