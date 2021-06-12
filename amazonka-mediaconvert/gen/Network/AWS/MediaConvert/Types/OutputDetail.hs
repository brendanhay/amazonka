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
-- Module      : Network.AWS.MediaConvert.Types.OutputDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.OutputDetail where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.VideoDetail

-- | Details regarding output
--
-- /See:/ 'newOutputDetail' smart constructor.
data OutputDetail = OutputDetail'
  { -- | Contains details about the output\'s video stream
    videoDetails :: Core.Maybe VideoDetail,
    -- | Duration in milliseconds
    durationInMs :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OutputDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'videoDetails', 'outputDetail_videoDetails' - Contains details about the output\'s video stream
--
-- 'durationInMs', 'outputDetail_durationInMs' - Duration in milliseconds
newOutputDetail ::
  OutputDetail
newOutputDetail =
  OutputDetail'
    { videoDetails = Core.Nothing,
      durationInMs = Core.Nothing
    }

-- | Contains details about the output\'s video stream
outputDetail_videoDetails :: Lens.Lens' OutputDetail (Core.Maybe VideoDetail)
outputDetail_videoDetails = Lens.lens (\OutputDetail' {videoDetails} -> videoDetails) (\s@OutputDetail' {} a -> s {videoDetails = a} :: OutputDetail)

-- | Duration in milliseconds
outputDetail_durationInMs :: Lens.Lens' OutputDetail (Core.Maybe Core.Int)
outputDetail_durationInMs = Lens.lens (\OutputDetail' {durationInMs} -> durationInMs) (\s@OutputDetail' {} a -> s {durationInMs = a} :: OutputDetail)

instance Core.FromJSON OutputDetail where
  parseJSON =
    Core.withObject
      "OutputDetail"
      ( \x ->
          OutputDetail'
            Core.<$> (x Core..:? "videoDetails")
            Core.<*> (x Core..:? "durationInMs")
      )

instance Core.Hashable OutputDetail

instance Core.NFData OutputDetail
