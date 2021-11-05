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
-- Module      : Amazonka.Transfer.Types.WorkflowStep
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.WorkflowStep where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transfer.Types.CopyStepDetails
import Amazonka.Transfer.Types.CustomStepDetails
import Amazonka.Transfer.Types.DeleteStepDetails
import Amazonka.Transfer.Types.TagStepDetails
import Amazonka.Transfer.Types.WorkflowStepType

-- | The basic building block of a workflow.
--
-- /See:/ 'newWorkflowStep' smart constructor.
data WorkflowStep = WorkflowStep'
  { -- | Details for a step that creates one or more tags.
    --
    -- You specify one or more tags: each tag contains a key\/value pair.
    tagStepDetails :: Prelude.Maybe TagStepDetails,
    -- | Details for a step that deletes the file.
    deleteStepDetails :: Prelude.Maybe DeleteStepDetails,
    -- | Details for a step that performs a file copy.
    --
    -- Consists of the following values:
    --
    -- -   A description
    --
    -- -   An S3 location for the destination of the file copy.
    --
    -- -   A flag that indicates whether or not to overwrite an existing file
    --     of the same name. The default is @FALSE@.
    copyStepDetails :: Prelude.Maybe CopyStepDetails,
    -- | Currently, the following step types are supported.
    --
    -- -   /Copy/: copy the file to another location
    --
    -- -   /Custom/: custom step with a lambda target
    --
    -- -   /Delete/: delete the file
    --
    -- -   /Tag/: add a tag to the file
    type' :: Prelude.Maybe WorkflowStepType,
    -- | Details for a step that invokes a lambda function.
    --
    -- Consists of the lambda function name, target, and timeout (in seconds).
    customStepDetails :: Prelude.Maybe CustomStepDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkflowStep' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagStepDetails', 'workflowStep_tagStepDetails' - Details for a step that creates one or more tags.
--
-- You specify one or more tags: each tag contains a key\/value pair.
--
-- 'deleteStepDetails', 'workflowStep_deleteStepDetails' - Details for a step that deletes the file.
--
-- 'copyStepDetails', 'workflowStep_copyStepDetails' - Details for a step that performs a file copy.
--
-- Consists of the following values:
--
-- -   A description
--
-- -   An S3 location for the destination of the file copy.
--
-- -   A flag that indicates whether or not to overwrite an existing file
--     of the same name. The default is @FALSE@.
--
-- 'type'', 'workflowStep_type' - Currently, the following step types are supported.
--
-- -   /Copy/: copy the file to another location
--
-- -   /Custom/: custom step with a lambda target
--
-- -   /Delete/: delete the file
--
-- -   /Tag/: add a tag to the file
--
-- 'customStepDetails', 'workflowStep_customStepDetails' - Details for a step that invokes a lambda function.
--
-- Consists of the lambda function name, target, and timeout (in seconds).
newWorkflowStep ::
  WorkflowStep
newWorkflowStep =
  WorkflowStep'
    { tagStepDetails = Prelude.Nothing,
      deleteStepDetails = Prelude.Nothing,
      copyStepDetails = Prelude.Nothing,
      type' = Prelude.Nothing,
      customStepDetails = Prelude.Nothing
    }

-- | Details for a step that creates one or more tags.
--
-- You specify one or more tags: each tag contains a key\/value pair.
workflowStep_tagStepDetails :: Lens.Lens' WorkflowStep (Prelude.Maybe TagStepDetails)
workflowStep_tagStepDetails = Lens.lens (\WorkflowStep' {tagStepDetails} -> tagStepDetails) (\s@WorkflowStep' {} a -> s {tagStepDetails = a} :: WorkflowStep)

-- | Details for a step that deletes the file.
workflowStep_deleteStepDetails :: Lens.Lens' WorkflowStep (Prelude.Maybe DeleteStepDetails)
workflowStep_deleteStepDetails = Lens.lens (\WorkflowStep' {deleteStepDetails} -> deleteStepDetails) (\s@WorkflowStep' {} a -> s {deleteStepDetails = a} :: WorkflowStep)

-- | Details for a step that performs a file copy.
--
-- Consists of the following values:
--
-- -   A description
--
-- -   An S3 location for the destination of the file copy.
--
-- -   A flag that indicates whether or not to overwrite an existing file
--     of the same name. The default is @FALSE@.
workflowStep_copyStepDetails :: Lens.Lens' WorkflowStep (Prelude.Maybe CopyStepDetails)
workflowStep_copyStepDetails = Lens.lens (\WorkflowStep' {copyStepDetails} -> copyStepDetails) (\s@WorkflowStep' {} a -> s {copyStepDetails = a} :: WorkflowStep)

-- | Currently, the following step types are supported.
--
-- -   /Copy/: copy the file to another location
--
-- -   /Custom/: custom step with a lambda target
--
-- -   /Delete/: delete the file
--
-- -   /Tag/: add a tag to the file
workflowStep_type :: Lens.Lens' WorkflowStep (Prelude.Maybe WorkflowStepType)
workflowStep_type = Lens.lens (\WorkflowStep' {type'} -> type') (\s@WorkflowStep' {} a -> s {type' = a} :: WorkflowStep)

-- | Details for a step that invokes a lambda function.
--
-- Consists of the lambda function name, target, and timeout (in seconds).
workflowStep_customStepDetails :: Lens.Lens' WorkflowStep (Prelude.Maybe CustomStepDetails)
workflowStep_customStepDetails = Lens.lens (\WorkflowStep' {customStepDetails} -> customStepDetails) (\s@WorkflowStep' {} a -> s {customStepDetails = a} :: WorkflowStep)

instance Core.FromJSON WorkflowStep where
  parseJSON =
    Core.withObject
      "WorkflowStep"
      ( \x ->
          WorkflowStep'
            Prelude.<$> (x Core..:? "TagStepDetails")
            Prelude.<*> (x Core..:? "DeleteStepDetails")
            Prelude.<*> (x Core..:? "CopyStepDetails")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "CustomStepDetails")
      )

instance Prelude.Hashable WorkflowStep

instance Prelude.NFData WorkflowStep

instance Core.ToJSON WorkflowStep where
  toJSON WorkflowStep' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TagStepDetails" Core..=)
              Prelude.<$> tagStepDetails,
            ("DeleteStepDetails" Core..=)
              Prelude.<$> deleteStepDetails,
            ("CopyStepDetails" Core..=)
              Prelude.<$> copyStepDetails,
            ("Type" Core..=) Prelude.<$> type',
            ("CustomStepDetails" Core..=)
              Prelude.<$> customStepDetails
          ]
      )
