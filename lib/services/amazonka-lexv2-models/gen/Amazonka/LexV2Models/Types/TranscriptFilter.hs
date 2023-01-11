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
-- Module      : Amazonka.LexV2Models.Types.TranscriptFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.TranscriptFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.LexTranscriptFilter
import qualified Amazonka.Prelude as Prelude

-- | The object representing the filter that Amazon Lex will use to select
-- the appropriate transcript.
--
-- /See:/ 'newTranscriptFilter' smart constructor.
data TranscriptFilter = TranscriptFilter'
  { -- | The object representing the filter that Amazon Lex will use to select
    -- the appropriate transcript when the transcript format is the Amazon Lex
    -- format.
    lexTranscriptFilter :: Prelude.Maybe LexTranscriptFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TranscriptFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lexTranscriptFilter', 'transcriptFilter_lexTranscriptFilter' - The object representing the filter that Amazon Lex will use to select
-- the appropriate transcript when the transcript format is the Amazon Lex
-- format.
newTranscriptFilter ::
  TranscriptFilter
newTranscriptFilter =
  TranscriptFilter'
    { lexTranscriptFilter =
        Prelude.Nothing
    }

-- | The object representing the filter that Amazon Lex will use to select
-- the appropriate transcript when the transcript format is the Amazon Lex
-- format.
transcriptFilter_lexTranscriptFilter :: Lens.Lens' TranscriptFilter (Prelude.Maybe LexTranscriptFilter)
transcriptFilter_lexTranscriptFilter = Lens.lens (\TranscriptFilter' {lexTranscriptFilter} -> lexTranscriptFilter) (\s@TranscriptFilter' {} a -> s {lexTranscriptFilter = a} :: TranscriptFilter)

instance Data.FromJSON TranscriptFilter where
  parseJSON =
    Data.withObject
      "TranscriptFilter"
      ( \x ->
          TranscriptFilter'
            Prelude.<$> (x Data..:? "lexTranscriptFilter")
      )

instance Prelude.Hashable TranscriptFilter where
  hashWithSalt _salt TranscriptFilter' {..} =
    _salt `Prelude.hashWithSalt` lexTranscriptFilter

instance Prelude.NFData TranscriptFilter where
  rnf TranscriptFilter' {..} =
    Prelude.rnf lexTranscriptFilter

instance Data.ToJSON TranscriptFilter where
  toJSON TranscriptFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("lexTranscriptFilter" Data..=)
              Prelude.<$> lexTranscriptFilter
          ]
      )
