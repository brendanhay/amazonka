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
-- Module      : Amazonka.LakeFormation.Types.WorkUnitRange
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.WorkUnitRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Defines the valid range of work unit IDs for querying the execution
-- service.
--
-- /See:/ 'newWorkUnitRange' smart constructor.
data WorkUnitRange = WorkUnitRange'
  { -- | Defines the maximum work unit ID in the range. The maximum value is
    -- inclusive.
    workUnitIdMax :: Prelude.Integer,
    -- | Defines the minimum work unit ID in the range.
    workUnitIdMin :: Prelude.Integer,
    -- | A work token used to query the execution service.
    workUnitToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkUnitRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workUnitIdMax', 'workUnitRange_workUnitIdMax' - Defines the maximum work unit ID in the range. The maximum value is
-- inclusive.
--
-- 'workUnitIdMin', 'workUnitRange_workUnitIdMin' - Defines the minimum work unit ID in the range.
--
-- 'workUnitToken', 'workUnitRange_workUnitToken' - A work token used to query the execution service.
newWorkUnitRange ::
  -- | 'workUnitIdMax'
  Prelude.Integer ->
  -- | 'workUnitIdMin'
  Prelude.Integer ->
  -- | 'workUnitToken'
  Prelude.Text ->
  WorkUnitRange
newWorkUnitRange
  pWorkUnitIdMax_
  pWorkUnitIdMin_
  pWorkUnitToken_ =
    WorkUnitRange'
      { workUnitIdMax = pWorkUnitIdMax_,
        workUnitIdMin = pWorkUnitIdMin_,
        workUnitToken = pWorkUnitToken_
      }

-- | Defines the maximum work unit ID in the range. The maximum value is
-- inclusive.
workUnitRange_workUnitIdMax :: Lens.Lens' WorkUnitRange Prelude.Integer
workUnitRange_workUnitIdMax = Lens.lens (\WorkUnitRange' {workUnitIdMax} -> workUnitIdMax) (\s@WorkUnitRange' {} a -> s {workUnitIdMax = a} :: WorkUnitRange)

-- | Defines the minimum work unit ID in the range.
workUnitRange_workUnitIdMin :: Lens.Lens' WorkUnitRange Prelude.Integer
workUnitRange_workUnitIdMin = Lens.lens (\WorkUnitRange' {workUnitIdMin} -> workUnitIdMin) (\s@WorkUnitRange' {} a -> s {workUnitIdMin = a} :: WorkUnitRange)

-- | A work token used to query the execution service.
workUnitRange_workUnitToken :: Lens.Lens' WorkUnitRange Prelude.Text
workUnitRange_workUnitToken = Lens.lens (\WorkUnitRange' {workUnitToken} -> workUnitToken) (\s@WorkUnitRange' {} a -> s {workUnitToken = a} :: WorkUnitRange)

instance Core.FromJSON WorkUnitRange where
  parseJSON =
    Core.withObject
      "WorkUnitRange"
      ( \x ->
          WorkUnitRange'
            Prelude.<$> (x Core..: "WorkUnitIdMax")
            Prelude.<*> (x Core..: "WorkUnitIdMin")
            Prelude.<*> (x Core..: "WorkUnitToken")
      )

instance Prelude.Hashable WorkUnitRange where
  hashWithSalt _salt WorkUnitRange' {..} =
    _salt `Prelude.hashWithSalt` workUnitIdMax
      `Prelude.hashWithSalt` workUnitIdMin
      `Prelude.hashWithSalt` workUnitToken

instance Prelude.NFData WorkUnitRange where
  rnf WorkUnitRange' {..} =
    Prelude.rnf workUnitIdMax
      `Prelude.seq` Prelude.rnf workUnitIdMin
      `Prelude.seq` Prelude.rnf workUnitToken
