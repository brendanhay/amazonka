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
-- Module      : Amazonka.Rekognition.Types.StreamProcessingStopSelector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.StreamProcessingStopSelector where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies when to stop processing the stream. You can specify a maximum
-- amount of time to process the video.
--
-- /See:/ 'newStreamProcessingStopSelector' smart constructor.
data StreamProcessingStopSelector = StreamProcessingStopSelector'
  { -- | Specifies the maximum amount of time in seconds that you want the stream
    -- to be processed. The largest amount of time is 2 minutes. The default is
    -- 10 seconds.
    maxDurationInSeconds :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamProcessingStopSelector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxDurationInSeconds', 'streamProcessingStopSelector_maxDurationInSeconds' - Specifies the maximum amount of time in seconds that you want the stream
-- to be processed. The largest amount of time is 2 minutes. The default is
-- 10 seconds.
newStreamProcessingStopSelector ::
  StreamProcessingStopSelector
newStreamProcessingStopSelector =
  StreamProcessingStopSelector'
    { maxDurationInSeconds =
        Prelude.Nothing
    }

-- | Specifies the maximum amount of time in seconds that you want the stream
-- to be processed. The largest amount of time is 2 minutes. The default is
-- 10 seconds.
streamProcessingStopSelector_maxDurationInSeconds :: Lens.Lens' StreamProcessingStopSelector (Prelude.Maybe Prelude.Natural)
streamProcessingStopSelector_maxDurationInSeconds = Lens.lens (\StreamProcessingStopSelector' {maxDurationInSeconds} -> maxDurationInSeconds) (\s@StreamProcessingStopSelector' {} a -> s {maxDurationInSeconds = a} :: StreamProcessingStopSelector)

instance
  Prelude.Hashable
    StreamProcessingStopSelector
  where
  hashWithSalt _salt StreamProcessingStopSelector' {..} =
    _salt `Prelude.hashWithSalt` maxDurationInSeconds

instance Prelude.NFData StreamProcessingStopSelector where
  rnf StreamProcessingStopSelector' {..} =
    Prelude.rnf maxDurationInSeconds

instance Data.ToJSON StreamProcessingStopSelector where
  toJSON StreamProcessingStopSelector' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxDurationInSeconds" Data..=)
              Prelude.<$> maxDurationInSeconds
          ]
      )
