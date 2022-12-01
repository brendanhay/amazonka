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
-- Module      : Amazonka.Transfer.Types.CustomStepDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.CustomStepDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Each step type has its own @StepDetails@ structure.
--
-- /See:/ 'newCustomStepDetails' smart constructor.
data CustomStepDetails = CustomStepDetails'
  { -- | The name of the step, used as an identifier.
    name :: Prelude.Maybe Prelude.Text,
    -- | Timeout, in seconds, for the step.
    timeoutSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The ARN for the lambda function that is being called.
    target :: Prelude.Maybe Prelude.Text,
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
-- Create a value of 'CustomStepDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'customStepDetails_name' - The name of the step, used as an identifier.
--
-- 'timeoutSeconds', 'customStepDetails_timeoutSeconds' - Timeout, in seconds, for the step.
--
-- 'target', 'customStepDetails_target' - The ARN for the lambda function that is being called.
--
-- 'sourceFileLocation', 'customStepDetails_sourceFileLocation' - Specifies which file to use as input to the workflow step: either the
-- output from the previous step, or the originally uploaded file for the
-- workflow.
--
-- -   Enter @${previous.file}@ to use the previous file as the input. In
--     this case, this workflow step uses the output file from the previous
--     workflow step as input. This is the default value.
--
-- -   Enter @${original.file}@ to use the originally-uploaded file
--     location as input for this step.
newCustomStepDetails ::
  CustomStepDetails
newCustomStepDetails =
  CustomStepDetails'
    { name = Prelude.Nothing,
      timeoutSeconds = Prelude.Nothing,
      target = Prelude.Nothing,
      sourceFileLocation = Prelude.Nothing
    }

-- | The name of the step, used as an identifier.
customStepDetails_name :: Lens.Lens' CustomStepDetails (Prelude.Maybe Prelude.Text)
customStepDetails_name = Lens.lens (\CustomStepDetails' {name} -> name) (\s@CustomStepDetails' {} a -> s {name = a} :: CustomStepDetails)

-- | Timeout, in seconds, for the step.
customStepDetails_timeoutSeconds :: Lens.Lens' CustomStepDetails (Prelude.Maybe Prelude.Natural)
customStepDetails_timeoutSeconds = Lens.lens (\CustomStepDetails' {timeoutSeconds} -> timeoutSeconds) (\s@CustomStepDetails' {} a -> s {timeoutSeconds = a} :: CustomStepDetails)

-- | The ARN for the lambda function that is being called.
customStepDetails_target :: Lens.Lens' CustomStepDetails (Prelude.Maybe Prelude.Text)
customStepDetails_target = Lens.lens (\CustomStepDetails' {target} -> target) (\s@CustomStepDetails' {} a -> s {target = a} :: CustomStepDetails)

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
customStepDetails_sourceFileLocation :: Lens.Lens' CustomStepDetails (Prelude.Maybe Prelude.Text)
customStepDetails_sourceFileLocation = Lens.lens (\CustomStepDetails' {sourceFileLocation} -> sourceFileLocation) (\s@CustomStepDetails' {} a -> s {sourceFileLocation = a} :: CustomStepDetails)

instance Core.FromJSON CustomStepDetails where
  parseJSON =
    Core.withObject
      "CustomStepDetails"
      ( \x ->
          CustomStepDetails'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "TimeoutSeconds")
            Prelude.<*> (x Core..:? "Target")
            Prelude.<*> (x Core..:? "SourceFileLocation")
      )

instance Prelude.Hashable CustomStepDetails where
  hashWithSalt _salt CustomStepDetails' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` timeoutSeconds
      `Prelude.hashWithSalt` target
      `Prelude.hashWithSalt` sourceFileLocation

instance Prelude.NFData CustomStepDetails where
  rnf CustomStepDetails' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf timeoutSeconds
      `Prelude.seq` Prelude.rnf target
      `Prelude.seq` Prelude.rnf sourceFileLocation

instance Core.ToJSON CustomStepDetails where
  toJSON CustomStepDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Name" Core..=) Prelude.<$> name,
            ("TimeoutSeconds" Core..=)
              Prelude.<$> timeoutSeconds,
            ("Target" Core..=) Prelude.<$> target,
            ("SourceFileLocation" Core..=)
              Prelude.<$> sourceFileLocation
          ]
      )
