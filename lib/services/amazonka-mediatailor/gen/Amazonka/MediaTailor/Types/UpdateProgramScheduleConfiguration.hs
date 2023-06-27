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
-- Module      : Amazonka.MediaTailor.Types.UpdateProgramScheduleConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.UpdateProgramScheduleConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types.ClipRange
import Amazonka.MediaTailor.Types.UpdateProgramTransition
import qualified Amazonka.Prelude as Prelude

-- | Schedule configuration parameters.
--
-- /See:/ 'newUpdateProgramScheduleConfiguration' smart constructor.
data UpdateProgramScheduleConfiguration = UpdateProgramScheduleConfiguration'
  { -- | Program clip range configuration.
    clipRange :: Prelude.Maybe ClipRange,
    -- | Program transition configuration.
    transition :: Prelude.Maybe UpdateProgramTransition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProgramScheduleConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clipRange', 'updateProgramScheduleConfiguration_clipRange' - Program clip range configuration.
--
-- 'transition', 'updateProgramScheduleConfiguration_transition' - Program transition configuration.
newUpdateProgramScheduleConfiguration ::
  UpdateProgramScheduleConfiguration
newUpdateProgramScheduleConfiguration =
  UpdateProgramScheduleConfiguration'
    { clipRange =
        Prelude.Nothing,
      transition = Prelude.Nothing
    }

-- | Program clip range configuration.
updateProgramScheduleConfiguration_clipRange :: Lens.Lens' UpdateProgramScheduleConfiguration (Prelude.Maybe ClipRange)
updateProgramScheduleConfiguration_clipRange = Lens.lens (\UpdateProgramScheduleConfiguration' {clipRange} -> clipRange) (\s@UpdateProgramScheduleConfiguration' {} a -> s {clipRange = a} :: UpdateProgramScheduleConfiguration)

-- | Program transition configuration.
updateProgramScheduleConfiguration_transition :: Lens.Lens' UpdateProgramScheduleConfiguration (Prelude.Maybe UpdateProgramTransition)
updateProgramScheduleConfiguration_transition = Lens.lens (\UpdateProgramScheduleConfiguration' {transition} -> transition) (\s@UpdateProgramScheduleConfiguration' {} a -> s {transition = a} :: UpdateProgramScheduleConfiguration)

instance
  Prelude.Hashable
    UpdateProgramScheduleConfiguration
  where
  hashWithSalt
    _salt
    UpdateProgramScheduleConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` clipRange
        `Prelude.hashWithSalt` transition

instance
  Prelude.NFData
    UpdateProgramScheduleConfiguration
  where
  rnf UpdateProgramScheduleConfiguration' {..} =
    Prelude.rnf clipRange
      `Prelude.seq` Prelude.rnf transition

instance
  Data.ToJSON
    UpdateProgramScheduleConfiguration
  where
  toJSON UpdateProgramScheduleConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClipRange" Data..=) Prelude.<$> clipRange,
            ("Transition" Data..=) Prelude.<$> transition
          ]
      )
