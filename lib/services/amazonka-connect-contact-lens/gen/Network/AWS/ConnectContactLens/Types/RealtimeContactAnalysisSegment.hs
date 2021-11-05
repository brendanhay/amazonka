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
-- Module      : Network.AWS.ConnectContactLens.Types.RealtimeContactAnalysisSegment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ConnectContactLens.Types.RealtimeContactAnalysisSegment where

import Network.AWS.ConnectContactLens.Types.Categories
import Network.AWS.ConnectContactLens.Types.Transcript
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An analyzed segment for a real-time analysis session.
--
-- /See:/ 'newRealtimeContactAnalysisSegment' smart constructor.
data RealtimeContactAnalysisSegment = RealtimeContactAnalysisSegment'
  { -- | The matched category rules.
    categories :: Prelude.Maybe Categories,
    -- | The analyzed transcript.
    transcript :: Prelude.Maybe Transcript
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
-- 'categories', 'realtimeContactAnalysisSegment_categories' - The matched category rules.
--
-- 'transcript', 'realtimeContactAnalysisSegment_transcript' - The analyzed transcript.
newRealtimeContactAnalysisSegment ::
  RealtimeContactAnalysisSegment
newRealtimeContactAnalysisSegment =
  RealtimeContactAnalysisSegment'
    { categories =
        Prelude.Nothing,
      transcript = Prelude.Nothing
    }

-- | The matched category rules.
realtimeContactAnalysisSegment_categories :: Lens.Lens' RealtimeContactAnalysisSegment (Prelude.Maybe Categories)
realtimeContactAnalysisSegment_categories = Lens.lens (\RealtimeContactAnalysisSegment' {categories} -> categories) (\s@RealtimeContactAnalysisSegment' {} a -> s {categories = a} :: RealtimeContactAnalysisSegment)

-- | The analyzed transcript.
realtimeContactAnalysisSegment_transcript :: Lens.Lens' RealtimeContactAnalysisSegment (Prelude.Maybe Transcript)
realtimeContactAnalysisSegment_transcript = Lens.lens (\RealtimeContactAnalysisSegment' {transcript} -> transcript) (\s@RealtimeContactAnalysisSegment' {} a -> s {transcript = a} :: RealtimeContactAnalysisSegment)

instance Core.FromJSON RealtimeContactAnalysisSegment where
  parseJSON =
    Core.withObject
      "RealtimeContactAnalysisSegment"
      ( \x ->
          RealtimeContactAnalysisSegment'
            Prelude.<$> (x Core..:? "Categories")
            Prelude.<*> (x Core..:? "Transcript")
      )

instance
  Prelude.Hashable
    RealtimeContactAnalysisSegment

instance
  Prelude.NFData
    RealtimeContactAnalysisSegment
