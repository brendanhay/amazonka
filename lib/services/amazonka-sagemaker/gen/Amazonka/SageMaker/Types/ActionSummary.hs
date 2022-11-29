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
-- Module      : Amazonka.SageMaker.Types.ActionSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ActionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ActionSource
import Amazonka.SageMaker.Types.ActionStatus

-- | Lists the properties of an /action/. An action represents an action or
-- activity. Some examples are a workflow step and a model deployment.
-- Generally, an action involves at least one input artifact or output
-- artifact.
--
-- /See:/ 'newActionSummary' smart constructor.
data ActionSummary = ActionSummary'
  { -- | The name of the action.
    actionName :: Prelude.Maybe Prelude.Text,
    -- | The type of the action.
    actionType :: Prelude.Maybe Prelude.Text,
    -- | The status of the action.
    status :: Prelude.Maybe ActionStatus,
    -- | When the action was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The source of the action.
    source :: Prelude.Maybe ActionSource,
    -- | The Amazon Resource Name (ARN) of the action.
    actionArn :: Prelude.Maybe Prelude.Text,
    -- | When the action was created.
    creationTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionName', 'actionSummary_actionName' - The name of the action.
--
-- 'actionType', 'actionSummary_actionType' - The type of the action.
--
-- 'status', 'actionSummary_status' - The status of the action.
--
-- 'lastModifiedTime', 'actionSummary_lastModifiedTime' - When the action was last modified.
--
-- 'source', 'actionSummary_source' - The source of the action.
--
-- 'actionArn', 'actionSummary_actionArn' - The Amazon Resource Name (ARN) of the action.
--
-- 'creationTime', 'actionSummary_creationTime' - When the action was created.
newActionSummary ::
  ActionSummary
newActionSummary =
  ActionSummary'
    { actionName = Prelude.Nothing,
      actionType = Prelude.Nothing,
      status = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      source = Prelude.Nothing,
      actionArn = Prelude.Nothing,
      creationTime = Prelude.Nothing
    }

-- | The name of the action.
actionSummary_actionName :: Lens.Lens' ActionSummary (Prelude.Maybe Prelude.Text)
actionSummary_actionName = Lens.lens (\ActionSummary' {actionName} -> actionName) (\s@ActionSummary' {} a -> s {actionName = a} :: ActionSummary)

-- | The type of the action.
actionSummary_actionType :: Lens.Lens' ActionSummary (Prelude.Maybe Prelude.Text)
actionSummary_actionType = Lens.lens (\ActionSummary' {actionType} -> actionType) (\s@ActionSummary' {} a -> s {actionType = a} :: ActionSummary)

-- | The status of the action.
actionSummary_status :: Lens.Lens' ActionSummary (Prelude.Maybe ActionStatus)
actionSummary_status = Lens.lens (\ActionSummary' {status} -> status) (\s@ActionSummary' {} a -> s {status = a} :: ActionSummary)

-- | When the action was last modified.
actionSummary_lastModifiedTime :: Lens.Lens' ActionSummary (Prelude.Maybe Prelude.UTCTime)
actionSummary_lastModifiedTime = Lens.lens (\ActionSummary' {lastModifiedTime} -> lastModifiedTime) (\s@ActionSummary' {} a -> s {lastModifiedTime = a} :: ActionSummary) Prelude.. Lens.mapping Core._Time

-- | The source of the action.
actionSummary_source :: Lens.Lens' ActionSummary (Prelude.Maybe ActionSource)
actionSummary_source = Lens.lens (\ActionSummary' {source} -> source) (\s@ActionSummary' {} a -> s {source = a} :: ActionSummary)

-- | The Amazon Resource Name (ARN) of the action.
actionSummary_actionArn :: Lens.Lens' ActionSummary (Prelude.Maybe Prelude.Text)
actionSummary_actionArn = Lens.lens (\ActionSummary' {actionArn} -> actionArn) (\s@ActionSummary' {} a -> s {actionArn = a} :: ActionSummary)

-- | When the action was created.
actionSummary_creationTime :: Lens.Lens' ActionSummary (Prelude.Maybe Prelude.UTCTime)
actionSummary_creationTime = Lens.lens (\ActionSummary' {creationTime} -> creationTime) (\s@ActionSummary' {} a -> s {creationTime = a} :: ActionSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON ActionSummary where
  parseJSON =
    Core.withObject
      "ActionSummary"
      ( \x ->
          ActionSummary'
            Prelude.<$> (x Core..:? "ActionName")
            Prelude.<*> (x Core..:? "ActionType")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "Source")
            Prelude.<*> (x Core..:? "ActionArn")
            Prelude.<*> (x Core..:? "CreationTime")
      )

instance Prelude.Hashable ActionSummary where
  hashWithSalt _salt ActionSummary' {..} =
    _salt `Prelude.hashWithSalt` actionName
      `Prelude.hashWithSalt` actionType
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` actionArn
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData ActionSummary where
  rnf ActionSummary' {..} =
    Prelude.rnf actionName
      `Prelude.seq` Prelude.rnf actionType
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf actionArn
      `Prelude.seq` Prelude.rnf creationTime
