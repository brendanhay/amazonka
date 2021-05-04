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
-- Module      : Network.AWS.CodePipeline.Types.ActionExecutionFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionExecutionFilter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Filter values for the action execution.
--
-- /See:/ 'newActionExecutionFilter' smart constructor.
data ActionExecutionFilter = ActionExecutionFilter'
  { -- | The pipeline execution ID used to filter action execution history.
    pipelineExecutionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ActionExecutionFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineExecutionId', 'actionExecutionFilter_pipelineExecutionId' - The pipeline execution ID used to filter action execution history.
newActionExecutionFilter ::
  ActionExecutionFilter
newActionExecutionFilter =
  ActionExecutionFilter'
    { pipelineExecutionId =
        Prelude.Nothing
    }

-- | The pipeline execution ID used to filter action execution history.
actionExecutionFilter_pipelineExecutionId :: Lens.Lens' ActionExecutionFilter (Prelude.Maybe Prelude.Text)
actionExecutionFilter_pipelineExecutionId = Lens.lens (\ActionExecutionFilter' {pipelineExecutionId} -> pipelineExecutionId) (\s@ActionExecutionFilter' {} a -> s {pipelineExecutionId = a} :: ActionExecutionFilter)

instance Prelude.Hashable ActionExecutionFilter

instance Prelude.NFData ActionExecutionFilter

instance Prelude.ToJSON ActionExecutionFilter where
  toJSON ActionExecutionFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("pipelineExecutionId" Prelude..=)
              Prelude.<$> pipelineExecutionId
          ]
      )
