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
-- Module      : Amazonka.CodeStar.Types.ProjectStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeStar.Types.ProjectStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An indication of whether a project creation or deletion is failed or
-- successful.
--
-- /See:/ 'newProjectStatus' smart constructor.
data ProjectStatus = ProjectStatus'
  { -- | In the case of a project creation or deletion failure, a reason for the
    -- failure.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The phase of completion for a project creation or deletion.
    state :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProjectStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reason', 'projectStatus_reason' - In the case of a project creation or deletion failure, a reason for the
-- failure.
--
-- 'state', 'projectStatus_state' - The phase of completion for a project creation or deletion.
newProjectStatus ::
  -- | 'state'
  Prelude.Text ->
  ProjectStatus
newProjectStatus pState_ =
  ProjectStatus'
    { reason = Prelude.Nothing,
      state = pState_
    }

-- | In the case of a project creation or deletion failure, a reason for the
-- failure.
projectStatus_reason :: Lens.Lens' ProjectStatus (Prelude.Maybe Prelude.Text)
projectStatus_reason = Lens.lens (\ProjectStatus' {reason} -> reason) (\s@ProjectStatus' {} a -> s {reason = a} :: ProjectStatus)

-- | The phase of completion for a project creation or deletion.
projectStatus_state :: Lens.Lens' ProjectStatus Prelude.Text
projectStatus_state = Lens.lens (\ProjectStatus' {state} -> state) (\s@ProjectStatus' {} a -> s {state = a} :: ProjectStatus)

instance Data.FromJSON ProjectStatus where
  parseJSON =
    Data.withObject
      "ProjectStatus"
      ( \x ->
          ProjectStatus'
            Prelude.<$> (x Data..:? "reason")
            Prelude.<*> (x Data..: "state")
      )

instance Prelude.Hashable ProjectStatus where
  hashWithSalt _salt ProjectStatus' {..} =
    _salt
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` state

instance Prelude.NFData ProjectStatus where
  rnf ProjectStatus' {..} =
    Prelude.rnf reason `Prelude.seq` Prelude.rnf state
