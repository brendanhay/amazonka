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
-- Module      : Network.AWS.MediaLive.Types.VideoSelectorProgramId
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.VideoSelectorProgramId where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Video Selector Program Id
--
-- /See:/ 'newVideoSelectorProgramId' smart constructor.
data VideoSelectorProgramId = VideoSelectorProgramId'
  { -- | Selects a specific program from within a multi-program transport stream.
    -- If the program doesn\'t exist, the first program within the transport
    -- stream will be selected by default.
    programId :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VideoSelectorProgramId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'programId', 'videoSelectorProgramId_programId' - Selects a specific program from within a multi-program transport stream.
-- If the program doesn\'t exist, the first program within the transport
-- stream will be selected by default.
newVideoSelectorProgramId ::
  VideoSelectorProgramId
newVideoSelectorProgramId =
  VideoSelectorProgramId'
    { programId =
        Prelude.Nothing
    }

-- | Selects a specific program from within a multi-program transport stream.
-- If the program doesn\'t exist, the first program within the transport
-- stream will be selected by default.
videoSelectorProgramId_programId :: Lens.Lens' VideoSelectorProgramId (Prelude.Maybe Prelude.Natural)
videoSelectorProgramId_programId = Lens.lens (\VideoSelectorProgramId' {programId} -> programId) (\s@VideoSelectorProgramId' {} a -> s {programId = a} :: VideoSelectorProgramId)

instance Core.FromJSON VideoSelectorProgramId where
  parseJSON =
    Core.withObject
      "VideoSelectorProgramId"
      ( \x ->
          VideoSelectorProgramId'
            Prelude.<$> (x Core..:? "programId")
      )

instance Prelude.Hashable VideoSelectorProgramId

instance Prelude.NFData VideoSelectorProgramId

instance Core.ToJSON VideoSelectorProgramId where
  toJSON VideoSelectorProgramId' {..} =
    Core.object
      ( Prelude.catMaybes
          [("programId" Core..=) Prelude.<$> programId]
      )
