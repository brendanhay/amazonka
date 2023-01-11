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
-- Module      : Amazonka.Transfer.Types.DeleteStepDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.DeleteStepDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The name of the step, used to identify the delete step.
--
-- /See:/ 'newDeleteStepDetails' smart constructor.
data DeleteStepDetails = DeleteStepDetails'
  { -- | The name of the step, used as an identifier.
    name :: Prelude.Maybe Prelude.Text,
    -- | Specifies which file to use as input to the workflow step: either the
    -- output from the previous step, or the originally uploaded file for the
    -- workflow.
    --
    -- -   Enter @${previous.file}@ to use the previous file as the input. In
    --     this case, this workflow step uses the output file from the previous
    --     workflow step as input. This is the default value.
    --
    -- -   Enter @${original.file}@ to use the originally-uploaded file
    --     location as input for this step.
    sourceFileLocation :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStepDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteStepDetails_name' - The name of the step, used as an identifier.
--
-- 'sourceFileLocation', 'deleteStepDetails_sourceFileLocation' - Specifies which file to use as input to the workflow step: either the
-- output from the previous step, or the originally uploaded file for the
-- workflow.
--
-- -   Enter @${previous.file}@ to use the previous file as the input. In
--     this case, this workflow step uses the output file from the previous
--     workflow step as input. This is the default value.
--
-- -   Enter @${original.file}@ to use the originally-uploaded file
--     location as input for this step.
newDeleteStepDetails ::
  DeleteStepDetails
newDeleteStepDetails =
  DeleteStepDetails'
    { name = Prelude.Nothing,
      sourceFileLocation = Prelude.Nothing
    }

-- | The name of the step, used as an identifier.
deleteStepDetails_name :: Lens.Lens' DeleteStepDetails (Prelude.Maybe Prelude.Text)
deleteStepDetails_name = Lens.lens (\DeleteStepDetails' {name} -> name) (\s@DeleteStepDetails' {} a -> s {name = a} :: DeleteStepDetails)

-- | Specifies which file to use as input to the workflow step: either the
-- output from the previous step, or the originally uploaded file for the
-- workflow.
--
-- -   Enter @${previous.file}@ to use the previous file as the input. In
--     this case, this workflow step uses the output file from the previous
--     workflow step as input. This is the default value.
--
-- -   Enter @${original.file}@ to use the originally-uploaded file
--     location as input for this step.
deleteStepDetails_sourceFileLocation :: Lens.Lens' DeleteStepDetails (Prelude.Maybe Prelude.Text)
deleteStepDetails_sourceFileLocation = Lens.lens (\DeleteStepDetails' {sourceFileLocation} -> sourceFileLocation) (\s@DeleteStepDetails' {} a -> s {sourceFileLocation = a} :: DeleteStepDetails)

instance Data.FromJSON DeleteStepDetails where
  parseJSON =
    Data.withObject
      "DeleteStepDetails"
      ( \x ->
          DeleteStepDetails'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "SourceFileLocation")
      )

instance Prelude.Hashable DeleteStepDetails where
  hashWithSalt _salt DeleteStepDetails' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` sourceFileLocation

instance Prelude.NFData DeleteStepDetails where
  rnf DeleteStepDetails' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf sourceFileLocation

instance Data.ToJSON DeleteStepDetails where
  toJSON DeleteStepDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("SourceFileLocation" Data..=)
              Prelude.<$> sourceFileLocation
          ]
      )
