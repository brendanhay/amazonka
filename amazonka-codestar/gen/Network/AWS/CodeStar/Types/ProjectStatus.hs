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
-- Module      : Network.AWS.CodeStar.Types.ProjectStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStar.Types.ProjectStatus where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON ProjectStatus where
  parseJSON =
    Prelude.withObject
      "ProjectStatus"
      ( \x ->
          ProjectStatus'
            Prelude.<$> (x Prelude..:? "reason")
            Prelude.<*> (x Prelude..: "state")
      )

instance Prelude.Hashable ProjectStatus

instance Prelude.NFData ProjectStatus
