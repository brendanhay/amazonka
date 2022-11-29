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
-- Module      : Amazonka.MediaLive.Types.Scte35ReturnToNetworkScheduleActionSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Scte35ReturnToNetworkScheduleActionSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Settings for a SCTE-35 return_to_network message.
--
-- /See:/ 'newScte35ReturnToNetworkScheduleActionSettings' smart constructor.
data Scte35ReturnToNetworkScheduleActionSettings = Scte35ReturnToNetworkScheduleActionSettings'
  { -- | The splice_event_id for the SCTE-35 splice_insert, as defined in
    -- SCTE-35.
    spliceEventId :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Scte35ReturnToNetworkScheduleActionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'spliceEventId', 'scte35ReturnToNetworkScheduleActionSettings_spliceEventId' - The splice_event_id for the SCTE-35 splice_insert, as defined in
-- SCTE-35.
newScte35ReturnToNetworkScheduleActionSettings ::
  -- | 'spliceEventId'
  Prelude.Natural ->
  Scte35ReturnToNetworkScheduleActionSettings
newScte35ReturnToNetworkScheduleActionSettings
  pSpliceEventId_ =
    Scte35ReturnToNetworkScheduleActionSettings'
      { spliceEventId =
          pSpliceEventId_
      }

-- | The splice_event_id for the SCTE-35 splice_insert, as defined in
-- SCTE-35.
scte35ReturnToNetworkScheduleActionSettings_spliceEventId :: Lens.Lens' Scte35ReturnToNetworkScheduleActionSettings Prelude.Natural
scte35ReturnToNetworkScheduleActionSettings_spliceEventId = Lens.lens (\Scte35ReturnToNetworkScheduleActionSettings' {spliceEventId} -> spliceEventId) (\s@Scte35ReturnToNetworkScheduleActionSettings' {} a -> s {spliceEventId = a} :: Scte35ReturnToNetworkScheduleActionSettings)

instance
  Core.FromJSON
    Scte35ReturnToNetworkScheduleActionSettings
  where
  parseJSON =
    Core.withObject
      "Scte35ReturnToNetworkScheduleActionSettings"
      ( \x ->
          Scte35ReturnToNetworkScheduleActionSettings'
            Prelude.<$> (x Core..: "spliceEventId")
      )

instance
  Prelude.Hashable
    Scte35ReturnToNetworkScheduleActionSettings
  where
  hashWithSalt
    _salt
    Scte35ReturnToNetworkScheduleActionSettings' {..} =
      _salt `Prelude.hashWithSalt` spliceEventId

instance
  Prelude.NFData
    Scte35ReturnToNetworkScheduleActionSettings
  where
  rnf Scte35ReturnToNetworkScheduleActionSettings' {..} =
    Prelude.rnf spliceEventId

instance
  Core.ToJSON
    Scte35ReturnToNetworkScheduleActionSettings
  where
  toJSON
    Scte35ReturnToNetworkScheduleActionSettings' {..} =
      Core.object
        ( Prelude.catMaybes
            [ Prelude.Just
                ("spliceEventId" Core..= spliceEventId)
            ]
        )
