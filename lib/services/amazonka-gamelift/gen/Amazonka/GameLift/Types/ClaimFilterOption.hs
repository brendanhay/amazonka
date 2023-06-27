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
-- Module      : Amazonka.GameLift.Types.ClaimFilterOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.ClaimFilterOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types.FilterInstanceStatus
import qualified Amazonka.Prelude as Prelude

-- | __This data type is used with the Amazon GameLift FleetIQ and game
-- server groups.__
--
-- Filters which game servers may be claimed when calling
-- @ClaimGameServer@.
--
-- /See:/ 'newClaimFilterOption' smart constructor.
data ClaimFilterOption = ClaimFilterOption'
  { -- | List of instance statuses that game servers may be claimed on. If
    -- provided, the list must contain the @ACTIVE@ status.
    instanceStatuses :: Prelude.Maybe [FilterInstanceStatus]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClaimFilterOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceStatuses', 'claimFilterOption_instanceStatuses' - List of instance statuses that game servers may be claimed on. If
-- provided, the list must contain the @ACTIVE@ status.
newClaimFilterOption ::
  ClaimFilterOption
newClaimFilterOption =
  ClaimFilterOption'
    { instanceStatuses =
        Prelude.Nothing
    }

-- | List of instance statuses that game servers may be claimed on. If
-- provided, the list must contain the @ACTIVE@ status.
claimFilterOption_instanceStatuses :: Lens.Lens' ClaimFilterOption (Prelude.Maybe [FilterInstanceStatus])
claimFilterOption_instanceStatuses = Lens.lens (\ClaimFilterOption' {instanceStatuses} -> instanceStatuses) (\s@ClaimFilterOption' {} a -> s {instanceStatuses = a} :: ClaimFilterOption) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable ClaimFilterOption where
  hashWithSalt _salt ClaimFilterOption' {..} =
    _salt `Prelude.hashWithSalt` instanceStatuses

instance Prelude.NFData ClaimFilterOption where
  rnf ClaimFilterOption' {..} =
    Prelude.rnf instanceStatuses

instance Data.ToJSON ClaimFilterOption where
  toJSON ClaimFilterOption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InstanceStatuses" Data..=)
              Prelude.<$> instanceStatuses
          ]
      )
