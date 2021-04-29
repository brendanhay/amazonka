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
-- Module      : Network.AWS.CodeCommit.Types.RepositoryTriggerExecutionFailure
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.RepositoryTriggerExecutionFailure where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A trigger failed to run.
--
-- /See:/ 'newRepositoryTriggerExecutionFailure' smart constructor.
data RepositoryTriggerExecutionFailure = RepositoryTriggerExecutionFailure'
  { -- | Message information about the trigger that did not run.
    failureMessage :: Prelude.Maybe Prelude.Text,
    -- | The name of the trigger that did not run.
    trigger :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      trigger = Prelude.Nothing
    }

-- | Message information about the trigger that did not run.
repositoryTriggerExecutionFailure_failureMessage :: Lens.Lens' RepositoryTriggerExecutionFailure (Prelude.Maybe Prelude.Text)
repositoryTriggerExecutionFailure_failureMessage = Lens.lens (\RepositoryTriggerExecutionFailure' {failureMessage} -> failureMessage) (\s@RepositoryTriggerExecutionFailure' {} a -> s {failureMessage = a} :: RepositoryTriggerExecutionFailure)

-- | The name of the trigger that did not run.
repositoryTriggerExecutionFailure_trigger :: Lens.Lens' RepositoryTriggerExecutionFailure (Prelude.Maybe Prelude.Text)
repositoryTriggerExecutionFailure_trigger = Lens.lens (\RepositoryTriggerExecutionFailure' {trigger} -> trigger) (\s@RepositoryTriggerExecutionFailure' {} a -> s {trigger = a} :: RepositoryTriggerExecutionFailure)

instance
  Prelude.FromJSON
    RepositoryTriggerExecutionFailure
  where
  parseJSON =
    Prelude.withObject
      "RepositoryTriggerExecutionFailure"
      ( \x ->
          RepositoryTriggerExecutionFailure'
            Prelude.<$> (x Prelude..:? "failureMessage")
            Prelude.<*> (x Prelude..:? "trigger")
      )

instance
  Prelude.Hashable
    RepositoryTriggerExecutionFailure

instance
  Prelude.NFData
    RepositoryTriggerExecutionFailure
