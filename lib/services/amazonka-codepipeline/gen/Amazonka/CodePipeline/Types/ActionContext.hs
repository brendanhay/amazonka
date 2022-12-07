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
-- Module      : Amazonka.CodePipeline.Types.ActionContext
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.ActionContext where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the context of an action in the stage of a pipeline to a job
-- worker.
--
-- /See:/ 'newActionContext' smart constructor.
data ActionContext = ActionContext'
  { -- | The name of the action in the context of a job.
    name :: Prelude.Maybe Prelude.Text,
    -- | The system-generated unique ID that corresponds to an action\'s
    -- execution.
    actionExecutionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActionContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'actionContext_name' - The name of the action in the context of a job.
--
-- 'actionExecutionId', 'actionContext_actionExecutionId' - The system-generated unique ID that corresponds to an action\'s
-- execution.
newActionContext ::
  ActionContext
newActionContext =
  ActionContext'
    { name = Prelude.Nothing,
      actionExecutionId = Prelude.Nothing
    }

-- | The name of the action in the context of a job.
actionContext_name :: Lens.Lens' ActionContext (Prelude.Maybe Prelude.Text)
actionContext_name = Lens.lens (\ActionContext' {name} -> name) (\s@ActionContext' {} a -> s {name = a} :: ActionContext)

-- | The system-generated unique ID that corresponds to an action\'s
-- execution.
actionContext_actionExecutionId :: Lens.Lens' ActionContext (Prelude.Maybe Prelude.Text)
actionContext_actionExecutionId = Lens.lens (\ActionContext' {actionExecutionId} -> actionExecutionId) (\s@ActionContext' {} a -> s {actionExecutionId = a} :: ActionContext)

instance Data.FromJSON ActionContext where
  parseJSON =
    Data.withObject
      "ActionContext"
      ( \x ->
          ActionContext'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "actionExecutionId")
      )

instance Prelude.Hashable ActionContext where
  hashWithSalt _salt ActionContext' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` actionExecutionId

instance Prelude.NFData ActionContext where
  rnf ActionContext' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf actionExecutionId
