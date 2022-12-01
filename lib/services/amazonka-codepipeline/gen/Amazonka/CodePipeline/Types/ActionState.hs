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
-- Module      : Amazonka.CodePipeline.Types.ActionState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.ActionState where

import Amazonka.CodePipeline.Types.ActionExecution
import Amazonka.CodePipeline.Types.ActionRevision
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents information about the state of an action.
--
-- /See:/ 'newActionState' smart constructor.
data ActionState = ActionState'
  { -- | The name of the action.
    actionName :: Prelude.Maybe Prelude.Text,
    -- | A URL link for more information about the state of the action, such as a
    -- deployment group details page.
    entityUrl :: Prelude.Maybe Prelude.Text,
    -- | A URL link for more information about the revision, such as a commit
    -- details page.
    revisionUrl :: Prelude.Maybe Prelude.Text,
    -- | Represents information about the version (or revision) of an action.
    currentRevision :: Prelude.Maybe ActionRevision,
    -- | Represents information about the run of an action.
    latestExecution :: Prelude.Maybe ActionExecution
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActionState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionName', 'actionState_actionName' - The name of the action.
--
-- 'entityUrl', 'actionState_entityUrl' - A URL link for more information about the state of the action, such as a
-- deployment group details page.
--
-- 'revisionUrl', 'actionState_revisionUrl' - A URL link for more information about the revision, such as a commit
-- details page.
--
-- 'currentRevision', 'actionState_currentRevision' - Represents information about the version (or revision) of an action.
--
-- 'latestExecution', 'actionState_latestExecution' - Represents information about the run of an action.
newActionState ::
  ActionState
newActionState =
  ActionState'
    { actionName = Prelude.Nothing,
      entityUrl = Prelude.Nothing,
      revisionUrl = Prelude.Nothing,
      currentRevision = Prelude.Nothing,
      latestExecution = Prelude.Nothing
    }

-- | The name of the action.
actionState_actionName :: Lens.Lens' ActionState (Prelude.Maybe Prelude.Text)
actionState_actionName = Lens.lens (\ActionState' {actionName} -> actionName) (\s@ActionState' {} a -> s {actionName = a} :: ActionState)

-- | A URL link for more information about the state of the action, such as a
-- deployment group details page.
actionState_entityUrl :: Lens.Lens' ActionState (Prelude.Maybe Prelude.Text)
actionState_entityUrl = Lens.lens (\ActionState' {entityUrl} -> entityUrl) (\s@ActionState' {} a -> s {entityUrl = a} :: ActionState)

-- | A URL link for more information about the revision, such as a commit
-- details page.
actionState_revisionUrl :: Lens.Lens' ActionState (Prelude.Maybe Prelude.Text)
actionState_revisionUrl = Lens.lens (\ActionState' {revisionUrl} -> revisionUrl) (\s@ActionState' {} a -> s {revisionUrl = a} :: ActionState)

-- | Represents information about the version (or revision) of an action.
actionState_currentRevision :: Lens.Lens' ActionState (Prelude.Maybe ActionRevision)
actionState_currentRevision = Lens.lens (\ActionState' {currentRevision} -> currentRevision) (\s@ActionState' {} a -> s {currentRevision = a} :: ActionState)

-- | Represents information about the run of an action.
actionState_latestExecution :: Lens.Lens' ActionState (Prelude.Maybe ActionExecution)
actionState_latestExecution = Lens.lens (\ActionState' {latestExecution} -> latestExecution) (\s@ActionState' {} a -> s {latestExecution = a} :: ActionState)

instance Core.FromJSON ActionState where
  parseJSON =
    Core.withObject
      "ActionState"
      ( \x ->
          ActionState'
            Prelude.<$> (x Core..:? "actionName")
            Prelude.<*> (x Core..:? "entityUrl")
            Prelude.<*> (x Core..:? "revisionUrl")
            Prelude.<*> (x Core..:? "currentRevision")
            Prelude.<*> (x Core..:? "latestExecution")
      )

instance Prelude.Hashable ActionState where
  hashWithSalt _salt ActionState' {..} =
    _salt `Prelude.hashWithSalt` actionName
      `Prelude.hashWithSalt` entityUrl
      `Prelude.hashWithSalt` revisionUrl
      `Prelude.hashWithSalt` currentRevision
      `Prelude.hashWithSalt` latestExecution

instance Prelude.NFData ActionState where
  rnf ActionState' {..} =
    Prelude.rnf actionName
      `Prelude.seq` Prelude.rnf entityUrl
      `Prelude.seq` Prelude.rnf revisionUrl
      `Prelude.seq` Prelude.rnf currentRevision
      `Prelude.seq` Prelude.rnf latestExecution
