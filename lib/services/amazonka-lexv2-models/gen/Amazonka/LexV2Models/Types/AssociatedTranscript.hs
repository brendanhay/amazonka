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
-- Module      : Amazonka.LexV2Models.Types.AssociatedTranscript
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.AssociatedTranscript where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The object containing information that associates the recommended
-- intent\/slot type with a conversation.
--
-- /See:/ 'newAssociatedTranscript' smart constructor.
data AssociatedTranscript = AssociatedTranscript'
  { -- | The content of the transcript that meets the search filter criteria. For
    -- the JSON format of the transcript, see
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/designing-output-format.html Output transcript format>.
    transcript :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociatedTranscript' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transcript', 'associatedTranscript_transcript' - The content of the transcript that meets the search filter criteria. For
-- the JSON format of the transcript, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/designing-output-format.html Output transcript format>.
newAssociatedTranscript ::
  AssociatedTranscript
newAssociatedTranscript =
  AssociatedTranscript' {transcript = Prelude.Nothing}

-- | The content of the transcript that meets the search filter criteria. For
-- the JSON format of the transcript, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/designing-output-format.html Output transcript format>.
associatedTranscript_transcript :: Lens.Lens' AssociatedTranscript (Prelude.Maybe Prelude.Text)
associatedTranscript_transcript = Lens.lens (\AssociatedTranscript' {transcript} -> transcript) (\s@AssociatedTranscript' {} a -> s {transcript = a} :: AssociatedTranscript)

instance Data.FromJSON AssociatedTranscript where
  parseJSON =
    Data.withObject
      "AssociatedTranscript"
      ( \x ->
          AssociatedTranscript'
            Prelude.<$> (x Data..:? "transcript")
      )

instance Prelude.Hashable AssociatedTranscript where
  hashWithSalt _salt AssociatedTranscript' {..} =
    _salt `Prelude.hashWithSalt` transcript

instance Prelude.NFData AssociatedTranscript where
  rnf AssociatedTranscript' {..} =
    Prelude.rnf transcript
