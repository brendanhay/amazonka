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
-- Module      : Network.AWS.SageMaker.Types.ActionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ActionSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.ActionSource
import Network.AWS.SageMaker.Types.ActionStatus

-- | Lists the properties of an /action/. An action represents an action or
-- activity. Some examples are a workflow step and a model deployment.
-- Generally, an action involves at least one input artifact or output
-- artifact.
--
-- /See:/ 'newActionSummary' smart constructor.
data ActionSummary = ActionSummary'
  { -- | The status of the action.
    status :: Prelude.Maybe ActionStatus,
    -- | When the action was created.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The name of the action.
    actionName :: Prelude.Maybe Prelude.Text,
    -- | The type of the action.
    actionType :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the action.
    actionArn :: Prelude.Maybe Prelude.Text,
    -- | The source of the action.
    source :: Prelude.Maybe ActionSource,
    -- | When the action was last modified.
    lastModifiedTime :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ActionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'actionSummary_status' - The status of the action.
--
-- 'creationTime', 'actionSummary_creationTime' - When the action was created.
--
-- 'actionName', 'actionSummary_actionName' - The name of the action.
--
-- 'actionType', 'actionSummary_actionType' - The type of the action.
--
-- 'actionArn', 'actionSummary_actionArn' - The Amazon Resource Name (ARN) of the action.
--
-- 'source', 'actionSummary_source' - The source of the action.
--
-- 'lastModifiedTime', 'actionSummary_lastModifiedTime' - When the action was last modified.
newActionSummary ::
  ActionSummary
newActionSummary =
  ActionSummary'
    { status = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      actionName = Prelude.Nothing,
      actionType = Prelude.Nothing,
      actionArn = Prelude.Nothing,
      source = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing
    }

-- | The status of the action.
actionSummary_status :: Lens.Lens' ActionSummary (Prelude.Maybe ActionStatus)
actionSummary_status = Lens.lens (\ActionSummary' {status} -> status) (\s@ActionSummary' {} a -> s {status = a} :: ActionSummary)

-- | When the action was created.
actionSummary_creationTime :: Lens.Lens' ActionSummary (Prelude.Maybe Prelude.UTCTime)
actionSummary_creationTime = Lens.lens (\ActionSummary' {creationTime} -> creationTime) (\s@ActionSummary' {} a -> s {creationTime = a} :: ActionSummary) Prelude.. Lens.mapping Prelude._Time

-- | The name of the action.
actionSummary_actionName :: Lens.Lens' ActionSummary (Prelude.Maybe Prelude.Text)
actionSummary_actionName = Lens.lens (\ActionSummary' {actionName} -> actionName) (\s@ActionSummary' {} a -> s {actionName = a} :: ActionSummary)

-- | The type of the action.
actionSummary_actionType :: Lens.Lens' ActionSummary (Prelude.Maybe Prelude.Text)
actionSummary_actionType = Lens.lens (\ActionSummary' {actionType} -> actionType) (\s@ActionSummary' {} a -> s {actionType = a} :: ActionSummary)

-- | The Amazon Resource Name (ARN) of the action.
actionSummary_actionArn :: Lens.Lens' ActionSummary (Prelude.Maybe Prelude.Text)
actionSummary_actionArn = Lens.lens (\ActionSummary' {actionArn} -> actionArn) (\s@ActionSummary' {} a -> s {actionArn = a} :: ActionSummary)

-- | The source of the action.
actionSummary_source :: Lens.Lens' ActionSummary (Prelude.Maybe ActionSource)
actionSummary_source = Lens.lens (\ActionSummary' {source} -> source) (\s@ActionSummary' {} a -> s {source = a} :: ActionSummary)

-- | When the action was last modified.
actionSummary_lastModifiedTime :: Lens.Lens' ActionSummary (Prelude.Maybe Prelude.UTCTime)
actionSummary_lastModifiedTime = Lens.lens (\ActionSummary' {lastModifiedTime} -> lastModifiedTime) (\s@ActionSummary' {} a -> s {lastModifiedTime = a} :: ActionSummary) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON ActionSummary where
  parseJSON =
    Prelude.withObject
      "ActionSummary"
      ( \x ->
          ActionSummary'
            Prelude.<$> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "ActionName")
            Prelude.<*> (x Prelude..:? "ActionType")
            Prelude.<*> (x Prelude..:? "ActionArn")
            Prelude.<*> (x Prelude..:? "Source")
            Prelude.<*> (x Prelude..:? "LastModifiedTime")
      )

instance Prelude.Hashable ActionSummary

instance Prelude.NFData ActionSummary
