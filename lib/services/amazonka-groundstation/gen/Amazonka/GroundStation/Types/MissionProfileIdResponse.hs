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
-- Module      : Amazonka.GroundStation.Types.MissionProfileIdResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.MissionProfileIdResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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

instance Data.FromJSON MissionProfileIdResponse where
  parseJSON =
    Data.withObject
      "MissionProfileIdResponse"
      ( \x ->
          MissionProfileIdResponse'
            Prelude.<$> (x Data..:? "missionProfileId")
      )

instance Prelude.Hashable MissionProfileIdResponse where
  hashWithSalt _salt MissionProfileIdResponse' {..} =
    _salt `Prelude.hashWithSalt` missionProfileId

instance Prelude.NFData MissionProfileIdResponse where
  rnf MissionProfileIdResponse' {..} =
    Prelude.rnf missionProfileId
