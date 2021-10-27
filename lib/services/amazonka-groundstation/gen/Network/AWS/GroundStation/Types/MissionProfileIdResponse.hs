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
-- Module      : Network.AWS.GroundStation.Types.MissionProfileIdResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GroundStation.Types.MissionProfileIdResponse where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- |
--
-- /See:/ 'newMissionProfileIdResponse' smart constructor.
data MissionProfileIdResponse = MissionProfileIdResponse'
  { -- | UUID of a mission profile.
    missionProfileId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MissionProfileIdResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'missionProfileId', 'missionProfileIdResponse_missionProfileId' - UUID of a mission profile.
newMissionProfileIdResponse ::
  MissionProfileIdResponse
newMissionProfileIdResponse =
  MissionProfileIdResponse'
    { missionProfileId =
        Prelude.Nothing
    }

-- | UUID of a mission profile.
missionProfileIdResponse_missionProfileId :: Lens.Lens' MissionProfileIdResponse (Prelude.Maybe Prelude.Text)
missionProfileIdResponse_missionProfileId = Lens.lens (\MissionProfileIdResponse' {missionProfileId} -> missionProfileId) (\s@MissionProfileIdResponse' {} a -> s {missionProfileId = a} :: MissionProfileIdResponse)

instance Core.FromJSON MissionProfileIdResponse where
  parseJSON =
    Core.withObject
      "MissionProfileIdResponse"
      ( \x ->
          MissionProfileIdResponse'
            Prelude.<$> (x Core..:? "missionProfileId")
      )

instance Prelude.Hashable MissionProfileIdResponse

instance Prelude.NFData MissionProfileIdResponse
