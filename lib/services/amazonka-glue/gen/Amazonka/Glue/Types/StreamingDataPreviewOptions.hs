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
-- Module      : Amazonka.Glue.Types.StreamingDataPreviewOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.StreamingDataPreviewOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies options related to data preview for viewing a sample of your
-- data.
--
-- /See:/ 'newStreamingDataPreviewOptions' smart constructor.
data StreamingDataPreviewOptions = StreamingDataPreviewOptions'
  { -- | The polling time in milliseconds.
    pollingTime :: Prelude.Maybe Prelude.Natural,
    -- | The limit to the number of records polled.
    recordPollingLimit :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamingDataPreviewOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pollingTime', 'streamingDataPreviewOptions_pollingTime' - The polling time in milliseconds.
--
-- 'recordPollingLimit', 'streamingDataPreviewOptions_recordPollingLimit' - The limit to the number of records polled.
newStreamingDataPreviewOptions ::
  StreamingDataPreviewOptions
newStreamingDataPreviewOptions =
  StreamingDataPreviewOptions'
    { pollingTime =
        Prelude.Nothing,
      recordPollingLimit = Prelude.Nothing
    }

-- | The polling time in milliseconds.
streamingDataPreviewOptions_pollingTime :: Lens.Lens' StreamingDataPreviewOptions (Prelude.Maybe Prelude.Natural)
streamingDataPreviewOptions_pollingTime = Lens.lens (\StreamingDataPreviewOptions' {pollingTime} -> pollingTime) (\s@StreamingDataPreviewOptions' {} a -> s {pollingTime = a} :: StreamingDataPreviewOptions)

-- | The limit to the number of records polled.
streamingDataPreviewOptions_recordPollingLimit :: Lens.Lens' StreamingDataPreviewOptions (Prelude.Maybe Prelude.Natural)
streamingDataPreviewOptions_recordPollingLimit = Lens.lens (\StreamingDataPreviewOptions' {recordPollingLimit} -> recordPollingLimit) (\s@StreamingDataPreviewOptions' {} a -> s {recordPollingLimit = a} :: StreamingDataPreviewOptions)

instance Data.FromJSON StreamingDataPreviewOptions where
  parseJSON =
    Data.withObject
      "StreamingDataPreviewOptions"
      ( \x ->
          StreamingDataPreviewOptions'
            Prelude.<$> (x Data..:? "PollingTime")
            Prelude.<*> (x Data..:? "RecordPollingLimit")
      )

instance Prelude.Hashable StreamingDataPreviewOptions where
  hashWithSalt _salt StreamingDataPreviewOptions' {..} =
    _salt `Prelude.hashWithSalt` pollingTime
      `Prelude.hashWithSalt` recordPollingLimit

instance Prelude.NFData StreamingDataPreviewOptions where
  rnf StreamingDataPreviewOptions' {..} =
    Prelude.rnf pollingTime
      `Prelude.seq` Prelude.rnf recordPollingLimit

instance Data.ToJSON StreamingDataPreviewOptions where
  toJSON StreamingDataPreviewOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PollingTime" Data..=) Prelude.<$> pollingTime,
            ("RecordPollingLimit" Data..=)
              Prelude.<$> recordPollingLimit
          ]
      )
