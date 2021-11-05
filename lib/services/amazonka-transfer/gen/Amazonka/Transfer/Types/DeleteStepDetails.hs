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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.DeleteStepDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The name of the step, used to identify the delete step.
--
-- /See:/ 'newDeleteStepDetails' smart constructor.
data DeleteStepDetails = DeleteStepDetails'
  { -- | The name of the step, used as an identifier.
    name :: Prelude.Maybe Prelude.Text
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
newDeleteStepDetails ::
  DeleteStepDetails
newDeleteStepDetails =
  DeleteStepDetails' {name = Prelude.Nothing}

-- | The name of the step, used as an identifier.
deleteStepDetails_name :: Lens.Lens' DeleteStepDetails (Prelude.Maybe Prelude.Text)
deleteStepDetails_name = Lens.lens (\DeleteStepDetails' {name} -> name) (\s@DeleteStepDetails' {} a -> s {name = a} :: DeleteStepDetails)

instance Core.FromJSON DeleteStepDetails where
  parseJSON =
    Core.withObject
      "DeleteStepDetails"
      ( \x ->
          DeleteStepDetails' Prelude.<$> (x Core..:? "Name")
      )

instance Prelude.Hashable DeleteStepDetails

instance Prelude.NFData DeleteStepDetails

instance Core.ToJSON DeleteStepDetails where
  toJSON DeleteStepDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [("Name" Core..=) Prelude.<$> name]
      )
