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
-- Module      : Network.AWS.CodePipeline.Types.StageContext
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.StageContext where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents information about a stage to a job worker.
--
-- /See:/ 'newStageContext' smart constructor.
data StageContext = StageContext'
  { -- | The name of the stage.
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StageContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'stageContext_name' - The name of the stage.
newStageContext ::
  StageContext
newStageContext = StageContext' {name = Core.Nothing}

-- | The name of the stage.
stageContext_name :: Lens.Lens' StageContext (Core.Maybe Core.Text)
stageContext_name = Lens.lens (\StageContext' {name} -> name) (\s@StageContext' {} a -> s {name = a} :: StageContext)

instance Core.FromJSON StageContext where
  parseJSON =
    Core.withObject
      "StageContext"
      (\x -> StageContext' Core.<$> (x Core..:? "name"))

instance Core.Hashable StageContext

instance Core.NFData StageContext
