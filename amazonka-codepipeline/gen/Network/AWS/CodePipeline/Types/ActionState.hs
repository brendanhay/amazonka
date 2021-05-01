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
-- Module      : Network.AWS.CodePipeline.Types.ActionState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionState where

import Network.AWS.CodePipeline.Types.ActionExecution
import Network.AWS.CodePipeline.Types.ActionRevision
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents information about the state of an action.
--
-- /See:/ 'newActionState' smart constructor.
data ActionState = ActionState'
  { -- | The name of the action.
    actionName :: Prelude.Maybe Prelude.Text,
    -- | Represents information about the run of an action.
    latestExecution :: Prelude.Maybe ActionExecution,
    -- | Represents information about the version (or revision) of an action.
    currentRevision :: Prelude.Maybe ActionRevision,
    -- | A URL link for more information about the state of the action, such as a
    -- deployment group details page.
    entityUrl :: Prelude.Maybe Prelude.Text,
    -- | A URL link for more information about the revision, such as a commit
    -- details page.
    revisionUrl :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'latestExecution', 'actionState_latestExecution' - Represents information about the run of an action.
--
-- 'currentRevision', 'actionState_currentRevision' - Represents information about the version (or revision) of an action.
--
-- 'entityUrl', 'actionState_entityUrl' - A URL link for more information about the state of the action, such as a
-- deployment group details page.
--
-- 'revisionUrl', 'actionState_revisionUrl' - A URL link for more information about the revision, such as a commit
-- details page.
newActionState ::
  ActionState
newActionState =
  ActionState'
    { actionName = Prelude.Nothing,
      latestExecution = Prelude.Nothing,
      currentRevision = Prelude.Nothing,
      entityUrl = Prelude.Nothing,
      revisionUrl = Prelude.Nothing
    }

-- | The name of the action.
actionState_actionName :: Lens.Lens' ActionState (Prelude.Maybe Prelude.Text)
actionState_actionName = Lens.lens (\ActionState' {actionName} -> actionName) (\s@ActionState' {} a -> s {actionName = a} :: ActionState)

-- | Represents information about the run of an action.
actionState_latestExecution :: Lens.Lens' ActionState (Prelude.Maybe ActionExecution)
actionState_latestExecution = Lens.lens (\ActionState' {latestExecution} -> latestExecution) (\s@ActionState' {} a -> s {latestExecution = a} :: ActionState)

-- | Represents information about the version (or revision) of an action.
actionState_currentRevision :: Lens.Lens' ActionState (Prelude.Maybe ActionRevision)
actionState_currentRevision = Lens.lens (\ActionState' {currentRevision} -> currentRevision) (\s@ActionState' {} a -> s {currentRevision = a} :: ActionState)

-- | A URL link for more information about the state of the action, such as a
-- deployment group details page.
actionState_entityUrl :: Lens.Lens' ActionState (Prelude.Maybe Prelude.Text)
actionState_entityUrl = Lens.lens (\ActionState' {entityUrl} -> entityUrl) (\s@ActionState' {} a -> s {entityUrl = a} :: ActionState)

-- | A URL link for more information about the revision, such as a commit
-- details page.
actionState_revisionUrl :: Lens.Lens' ActionState (Prelude.Maybe Prelude.Text)
actionState_revisionUrl = Lens.lens (\ActionState' {revisionUrl} -> revisionUrl) (\s@ActionState' {} a -> s {revisionUrl = a} :: ActionState)

instance Prelude.FromJSON ActionState where
  parseJSON =
    Prelude.withObject
      "ActionState"
      ( \x ->
          ActionState'
            Prelude.<$> (x Prelude..:? "actionName")
            Prelude.<*> (x Prelude..:? "latestExecution")
            Prelude.<*> (x Prelude..:? "currentRevision")
            Prelude.<*> (x Prelude..:? "entityUrl")
            Prelude.<*> (x Prelude..:? "revisionUrl")
      )

instance Prelude.Hashable ActionState

instance Prelude.NFData ActionState
