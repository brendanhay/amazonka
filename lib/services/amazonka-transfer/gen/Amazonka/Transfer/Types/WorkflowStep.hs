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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.WorkflowStep where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transfer.Types.CopyStepDetails
import Amazonka.Transfer.Types.CustomStepDetails
import Amazonka.Transfer.Types.DecryptStepDetails
import Amazonka.Transfer.Types.DeleteStepDetails
import Amazonka.Transfer.Types.TagStepDetails
import Amazonka.Transfer.Types.WorkflowStepType

-- | The basic building block of a workflow.
--
-- /See:/ 'newWorkflowStep' smart constructor.
data WorkflowStep = WorkflowStep'
  { -- | Details for a step that performs a file copy.
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
    -- | Details for a step that invokes a lambda function.
    --
    -- Consists of the lambda function name, target, and timeout (in seconds).
    customStepDetails :: Prelude.Maybe CustomStepDetails,
    decryptStepDetails :: Prelude.Maybe DecryptStepDetails,
    -- | Details for a step that deletes the file.
    deleteStepDetails :: Prelude.Maybe DeleteStepDetails,
    -- | Details for a step that creates one or more tags.
    --
    -- You specify one or more tags: each tag contains a key\/value pair.
    tagStepDetails :: Prelude.Maybe TagStepDetails,
    -- | Currently, the following step types are supported.
    --
    -- -   /COPY/: Copy the file to another location.
    --
    -- -   /CUSTOM/: Perform a custom step with an Lambda function target.
    --
    -- -   /DELETE/: Delete the file.
    --
    -- -   /TAG/: Add a tag to the file.
    type' :: Prelude.Maybe WorkflowStepType
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
-- 'customStepDetails', 'workflowStep_customStepDetails' - Details for a step that invokes a lambda function.
--
-- Consists of the lambda function name, target, and timeout (in seconds).
--
-- 'decryptStepDetails', 'workflowStep_decryptStepDetails' - Undocumented member.
--
-- 'deleteStepDetails', 'workflowStep_deleteStepDetails' - Details for a step that deletes the file.
--
-- 'tagStepDetails', 'workflowStep_tagStepDetails' - Details for a step that creates one or more tags.
--
-- You specify one or more tags: each tag contains a key\/value pair.
--
-- 'type'', 'workflowStep_type' - Currently, the following step types are supported.
--
-- -   /COPY/: Copy the file to another location.
--
-- -   /CUSTOM/: Perform a custom step with an Lambda function target.
--
-- -   /DELETE/: Delete the file.
--
-- -   /TAG/: Add a tag to the file.
newWorkflowStep ::
  WorkflowStep
newWorkflowStep =
  WorkflowStep'
    { copyStepDetails = Prelude.Nothing,
      customStepDetails = Prelude.Nothing,
      decryptStepDetails = Prelude.Nothing,
      deleteStepDetails = Prelude.Nothing,
      tagStepDetails = Prelude.Nothing,
      type' = Prelude.Nothing
    }

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

-- | Details for a step that invokes a lambda function.
--
-- Consists of the lambda function name, target, and timeout (in seconds).
workflowStep_customStepDetails :: Lens.Lens' WorkflowStep (Prelude.Maybe CustomStepDetails)
workflowStep_customStepDetails = Lens.lens (\WorkflowStep' {customStepDetails} -> customStepDetails) (\s@WorkflowStep' {} a -> s {customStepDetails = a} :: WorkflowStep)

-- | Undocumented member.
workflowStep_decryptStepDetails :: Lens.Lens' WorkflowStep (Prelude.Maybe DecryptStepDetails)
workflowStep_decryptStepDetails = Lens.lens (\WorkflowStep' {decryptStepDetails} -> decryptStepDetails) (\s@WorkflowStep' {} a -> s {decryptStepDetails = a} :: WorkflowStep)

-- | Details for a step that deletes the file.
workflowStep_deleteStepDetails :: Lens.Lens' WorkflowStep (Prelude.Maybe DeleteStepDetails)
workflowStep_deleteStepDetails = Lens.lens (\WorkflowStep' {deleteStepDetails} -> deleteStepDetails) (\s@WorkflowStep' {} a -> s {deleteStepDetails = a} :: WorkflowStep)

-- | Details for a step that creates one or more tags.
--
-- You specify one or more tags: each tag contains a key\/value pair.
workflowStep_tagStepDetails :: Lens.Lens' WorkflowStep (Prelude.Maybe TagStepDetails)
workflowStep_tagStepDetails = Lens.lens (\WorkflowStep' {tagStepDetails} -> tagStepDetails) (\s@WorkflowStep' {} a -> s {tagStepDetails = a} :: WorkflowStep)

-- | Currently, the following step types are supported.
--
-- -   /COPY/: Copy the file to another location.
--
-- -   /CUSTOM/: Perform a custom step with an Lambda function target.
--
-- -   /DELETE/: Delete the file.
--
-- -   /TAG/: Add a tag to the file.
workflowStep_type :: Lens.Lens' WorkflowStep (Prelude.Maybe WorkflowStepType)
workflowStep_type = Lens.lens (\WorkflowStep' {type'} -> type') (\s@WorkflowStep' {} a -> s {type' = a} :: WorkflowStep)

instance Data.FromJSON WorkflowStep where
  parseJSON =
    Data.withObject
      "WorkflowStep"
      ( \x ->
          WorkflowStep'
            Prelude.<$> (x Data..:? "CopyStepDetails")
            Prelude.<*> (x Data..:? "CustomStepDetails")
            Prelude.<*> (x Data..:? "DecryptStepDetails")
            Prelude.<*> (x Data..:? "DeleteStepDetails")
            Prelude.<*> (x Data..:? "TagStepDetails")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable WorkflowStep where
  hashWithSalt _salt WorkflowStep' {..} =
    _salt
      `Prelude.hashWithSalt` copyStepDetails
      `Prelude.hashWithSalt` customStepDetails
      `Prelude.hashWithSalt` decryptStepDetails
      `Prelude.hashWithSalt` deleteStepDetails
      `Prelude.hashWithSalt` tagStepDetails
      `Prelude.hashWithSalt` type'

instance Prelude.NFData WorkflowStep where
  rnf WorkflowStep' {..} =
    Prelude.rnf copyStepDetails
      `Prelude.seq` Prelude.rnf customStepDetails
      `Prelude.seq` Prelude.rnf decryptStepDetails
      `Prelude.seq` Prelude.rnf deleteStepDetails
      `Prelude.seq` Prelude.rnf tagStepDetails
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON WorkflowStep where
  toJSON WorkflowStep' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CopyStepDetails" Data..=)
              Prelude.<$> copyStepDetails,
            ("CustomStepDetails" Data..=)
              Prelude.<$> customStepDetails,
            ("DecryptStepDetails" Data..=)
              Prelude.<$> decryptStepDetails,
            ("DeleteStepDetails" Data..=)
              Prelude.<$> deleteStepDetails,
            ("TagStepDetails" Data..=)
              Prelude.<$> tagStepDetails,
            ("Type" Data..=) Prelude.<$> type'
          ]
      )
