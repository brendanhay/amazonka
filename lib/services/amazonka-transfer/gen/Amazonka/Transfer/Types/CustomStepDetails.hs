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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.CustomStepDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
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
    target :: Prelude.Maybe Prelude.Text
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
newCustomStepDetails ::
  CustomStepDetails
newCustomStepDetails =
  CustomStepDetails'
    { name = Prelude.Nothing,
      timeoutSeconds = Prelude.Nothing,
      target = Prelude.Nothing
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

instance Core.FromJSON CustomStepDetails where
  parseJSON =
    Core.withObject
      "CustomStepDetails"
      ( \x ->
          CustomStepDetails'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "TimeoutSeconds")
            Prelude.<*> (x Core..:? "Target")
      )

instance Prelude.Hashable CustomStepDetails

instance Prelude.NFData CustomStepDetails

instance Core.ToJSON CustomStepDetails where
  toJSON CustomStepDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Name" Core..=) Prelude.<$> name,
            ("TimeoutSeconds" Core..=)
              Prelude.<$> timeoutSeconds,
            ("Target" Core..=) Prelude.<$> target
          ]
      )
