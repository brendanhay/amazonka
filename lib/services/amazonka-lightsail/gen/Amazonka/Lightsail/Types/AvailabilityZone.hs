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
-- Module      : Amazonka.Lightsail.Types.AvailabilityZone
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.AvailabilityZone where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an Availability Zone. This is returned only as part of a
-- @GetRegions@ request.
--
-- /See:/ 'newAvailabilityZone' smart constructor.
data AvailabilityZone = AvailabilityZone'
  { -- | The state of the Availability Zone.
    state :: Prelude.Maybe Prelude.Text,
    -- | The name of the Availability Zone. The format is @us-east-2a@
    -- (case-sensitive).
    zoneName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AvailabilityZone' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'availabilityZone_state' - The state of the Availability Zone.
--
-- 'zoneName', 'availabilityZone_zoneName' - The name of the Availability Zone. The format is @us-east-2a@
-- (case-sensitive).
newAvailabilityZone ::
  AvailabilityZone
newAvailabilityZone =
  AvailabilityZone'
    { state = Prelude.Nothing,
      zoneName = Prelude.Nothing
    }

-- | The state of the Availability Zone.
availabilityZone_state :: Lens.Lens' AvailabilityZone (Prelude.Maybe Prelude.Text)
availabilityZone_state = Lens.lens (\AvailabilityZone' {state} -> state) (\s@AvailabilityZone' {} a -> s {state = a} :: AvailabilityZone)

-- | The name of the Availability Zone. The format is @us-east-2a@
-- (case-sensitive).
availabilityZone_zoneName :: Lens.Lens' AvailabilityZone (Prelude.Maybe Prelude.Text)
availabilityZone_zoneName = Lens.lens (\AvailabilityZone' {zoneName} -> zoneName) (\s@AvailabilityZone' {} a -> s {zoneName = a} :: AvailabilityZone)

instance Data.FromJSON AvailabilityZone where
  parseJSON =
    Data.withObject
      "AvailabilityZone"
      ( \x ->
          AvailabilityZone'
            Prelude.<$> (x Data..:? "state")
            Prelude.<*> (x Data..:? "zoneName")
      )

instance Prelude.Hashable AvailabilityZone where
  hashWithSalt _salt AvailabilityZone' {..} =
    _salt
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` zoneName

instance Prelude.NFData AvailabilityZone where
  rnf AvailabilityZone' {..} =
    Prelude.rnf state
      `Prelude.seq` Prelude.rnf zoneName
