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
-- Module      : Network.AWS.CodeCommit.Types.RepositoryTriggerExecutionFailure
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.RepositoryTriggerExecutionFailure where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A trigger failed to run.
--
-- /See:/ 'newRepositoryTriggerExecutionFailure' smart constructor.
data RepositoryTriggerExecutionFailure = RepositoryTriggerExecutionFailure'
  { -- | Message information about the trigger that did not run.
    failureMessage :: Core.Maybe Core.Text,
    -- | The name of the trigger that did not run.
    trigger :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RepositoryTriggerExecutionFailure' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureMessage', 'repositoryTriggerExecutionFailure_failureMessage' - Message information about the trigger that did not run.
--
-- 'trigger', 'repositoryTriggerExecutionFailure_trigger' - The name of the trigger that did not run.
newRepositoryTriggerExecutionFailure ::
  RepositoryTriggerExecutionFailure
newRepositoryTriggerExecutionFailure =
  RepositoryTriggerExecutionFailure'
    { failureMessage =
        Core.Nothing,
      trigger = Core.Nothing
    }

-- | Message information about the trigger that did not run.
repositoryTriggerExecutionFailure_failureMessage :: Lens.Lens' RepositoryTriggerExecutionFailure (Core.Maybe Core.Text)
repositoryTriggerExecutionFailure_failureMessage = Lens.lens (\RepositoryTriggerExecutionFailure' {failureMessage} -> failureMessage) (\s@RepositoryTriggerExecutionFailure' {} a -> s {failureMessage = a} :: RepositoryTriggerExecutionFailure)

-- | The name of the trigger that did not run.
repositoryTriggerExecutionFailure_trigger :: Lens.Lens' RepositoryTriggerExecutionFailure (Core.Maybe Core.Text)
repositoryTriggerExecutionFailure_trigger = Lens.lens (\RepositoryTriggerExecutionFailure' {trigger} -> trigger) (\s@RepositoryTriggerExecutionFailure' {} a -> s {trigger = a} :: RepositoryTriggerExecutionFailure)

instance
  Core.FromJSON
    RepositoryTriggerExecutionFailure
  where
  parseJSON =
    Core.withObject
      "RepositoryTriggerExecutionFailure"
      ( \x ->
          RepositoryTriggerExecutionFailure'
            Core.<$> (x Core..:? "failureMessage")
            Core.<*> (x Core..:? "trigger")
      )

instance
  Core.Hashable
    RepositoryTriggerExecutionFailure

instance
  Core.NFData
    RepositoryTriggerExecutionFailure
