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
-- Module      : Amazonka.MediaConvert.Types.OutputDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.OutputDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.VideoDetail
import qualified Amazonka.Prelude as Prelude

-- | Details regarding output
--
-- /See:/ 'newOutputDetail' smart constructor.
data OutputDetail = OutputDetail'
  { -- | Duration in milliseconds
    durationInMs :: Prelude.Maybe Prelude.Int,
    -- | Contains details about the output\'s video stream
    videoDetails :: Prelude.Maybe VideoDetail
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutputDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'durationInMs', 'outputDetail_durationInMs' - Duration in milliseconds
--
-- 'videoDetails', 'outputDetail_videoDetails' - Contains details about the output\'s video stream
newOutputDetail ::
  OutputDetail
newOutputDetail =
  OutputDetail'
    { durationInMs = Prelude.Nothing,
      videoDetails = Prelude.Nothing
    }

-- | Duration in milliseconds
outputDetail_durationInMs :: Lens.Lens' OutputDetail (Prelude.Maybe Prelude.Int)
outputDetail_durationInMs = Lens.lens (\OutputDetail' {durationInMs} -> durationInMs) (\s@OutputDetail' {} a -> s {durationInMs = a} :: OutputDetail)

-- | Contains details about the output\'s video stream
outputDetail_videoDetails :: Lens.Lens' OutputDetail (Prelude.Maybe VideoDetail)
outputDetail_videoDetails = Lens.lens (\OutputDetail' {videoDetails} -> videoDetails) (\s@OutputDetail' {} a -> s {videoDetails = a} :: OutputDetail)

instance Data.FromJSON OutputDetail where
  parseJSON =
    Data.withObject
      "OutputDetail"
      ( \x ->
          OutputDetail'
            Prelude.<$> (x Data..:? "durationInMs")
            Prelude.<*> (x Data..:? "videoDetails")
      )

instance Prelude.Hashable OutputDetail where
  hashWithSalt _salt OutputDetail' {..} =
    _salt
      `Prelude.hashWithSalt` durationInMs
      `Prelude.hashWithSalt` videoDetails

instance Prelude.NFData OutputDetail where
  rnf OutputDetail' {..} =
    Prelude.rnf durationInMs
      `Prelude.seq` Prelude.rnf videoDetails
