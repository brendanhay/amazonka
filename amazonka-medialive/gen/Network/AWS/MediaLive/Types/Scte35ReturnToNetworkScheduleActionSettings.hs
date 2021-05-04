{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaLive.Types.Scte35ReturnToNetworkScheduleActionSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35ReturnToNetworkScheduleActionSettings where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Settings for a SCTE-35 return_to_network message.
--
-- /See:/ 'newScte35ReturnToNetworkScheduleActionSettings' smart constructor.
data Scte35ReturnToNetworkScheduleActionSettings = Scte35ReturnToNetworkScheduleActionSettings'
  { -- | The splice_event_id for the SCTE-35 splice_insert, as defined in
    -- SCTE-35.
    spliceEventId :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.FromJSON
    Scte35ReturnToNetworkScheduleActionSettings
  where
  parseJSON =
    Prelude.withObject
      "Scte35ReturnToNetworkScheduleActionSettings"
      ( \x ->
          Scte35ReturnToNetworkScheduleActionSettings'
            Prelude.<$> (x Prelude..: "spliceEventId")
      )

instance
  Prelude.Hashable
    Scte35ReturnToNetworkScheduleActionSettings

instance
  Prelude.NFData
    Scte35ReturnToNetworkScheduleActionSettings

instance
  Prelude.ToJSON
    Scte35ReturnToNetworkScheduleActionSettings
  where
  toJSON
    Scte35ReturnToNetworkScheduleActionSettings' {..} =
      Prelude.object
        ( Prelude.catMaybes
            [ Prelude.Just
                ("spliceEventId" Prelude..= spliceEventId)
            ]
        )
