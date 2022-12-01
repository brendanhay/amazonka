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
-- Module      : Amazonka.ConnectContactLens.Types.RealtimeContactAnalysisSegment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectContactLens.Types.RealtimeContactAnalysisSegment where

import Amazonka.ConnectContactLens.Types.Categories
import Amazonka.ConnectContactLens.Types.Transcript
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An analyzed segment for a real-time analysis session.
--
-- /See:/ 'newRealtimeContactAnalysisSegment' smart constructor.
data RealtimeContactAnalysisSegment = RealtimeContactAnalysisSegment'
  { -- | The analyzed transcript.
    transcript :: Prelude.Maybe Transcript,
    -- | The matched category rules.
    categories :: Prelude.Maybe Categories
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RealtimeContactAnalysisSegment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transcript', 'realtimeContactAnalysisSegment_transcript' - The analyzed transcript.
--
-- 'categories', 'realtimeContactAnalysisSegment_categories' - The matched category rules.
newRealtimeContactAnalysisSegment ::
  RealtimeContactAnalysisSegment
newRealtimeContactAnalysisSegment =
  RealtimeContactAnalysisSegment'
    { transcript =
        Prelude.Nothing,
      categories = Prelude.Nothing
    }

-- | The analyzed transcript.
realtimeContactAnalysisSegment_transcript :: Lens.Lens' RealtimeContactAnalysisSegment (Prelude.Maybe Transcript)
realtimeContactAnalysisSegment_transcript = Lens.lens (\RealtimeContactAnalysisSegment' {transcript} -> transcript) (\s@RealtimeContactAnalysisSegment' {} a -> s {transcript = a} :: RealtimeContactAnalysisSegment)

-- | The matched category rules.
realtimeContactAnalysisSegment_categories :: Lens.Lens' RealtimeContactAnalysisSegment (Prelude.Maybe Categories)
realtimeContactAnalysisSegment_categories = Lens.lens (\RealtimeContactAnalysisSegment' {categories} -> categories) (\s@RealtimeContactAnalysisSegment' {} a -> s {categories = a} :: RealtimeContactAnalysisSegment)

instance Core.FromJSON RealtimeContactAnalysisSegment where
  parseJSON =
    Core.withObject
      "RealtimeContactAnalysisSegment"
      ( \x ->
          RealtimeContactAnalysisSegment'
            Prelude.<$> (x Core..:? "Transcript")
            Prelude.<*> (x Core..:? "Categories")
      )

instance
  Prelude.Hashable
    RealtimeContactAnalysisSegment
  where
  hashWithSalt
    _salt
    RealtimeContactAnalysisSegment' {..} =
      _salt `Prelude.hashWithSalt` transcript
        `Prelude.hashWithSalt` categories

instance
  Prelude.NFData
    RealtimeContactAnalysisSegment
  where
  rnf RealtimeContactAnalysisSegment' {..} =
    Prelude.rnf transcript
      `Prelude.seq` Prelude.rnf categories
