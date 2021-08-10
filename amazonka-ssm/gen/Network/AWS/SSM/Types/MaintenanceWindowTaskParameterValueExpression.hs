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
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowTaskParameterValueExpression
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowTaskParameterValueExpression where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Defines the values for a task parameter.
--
-- /See:/ 'newMaintenanceWindowTaskParameterValueExpression' smart constructor.
data MaintenanceWindowTaskParameterValueExpression = MaintenanceWindowTaskParameterValueExpression'
  { -- | This field contains an array of 0 or more strings, each 1 to 255
    -- characters in length.
    values :: Prelude.Maybe (Core.Sensitive [Core.Sensitive Prelude.Text])
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MaintenanceWindowTaskParameterValueExpression' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'maintenanceWindowTaskParameterValueExpression_values' - This field contains an array of 0 or more strings, each 1 to 255
-- characters in length.
newMaintenanceWindowTaskParameterValueExpression ::
  MaintenanceWindowTaskParameterValueExpression
newMaintenanceWindowTaskParameterValueExpression =
  MaintenanceWindowTaskParameterValueExpression'
    { values =
        Prelude.Nothing
    }

-- | This field contains an array of 0 or more strings, each 1 to 255
-- characters in length.
maintenanceWindowTaskParameterValueExpression_values :: Lens.Lens' MaintenanceWindowTaskParameterValueExpression (Prelude.Maybe [Prelude.Text])
maintenanceWindowTaskParameterValueExpression_values = Lens.lens (\MaintenanceWindowTaskParameterValueExpression' {values} -> values) (\s@MaintenanceWindowTaskParameterValueExpression' {} a -> s {values = a} :: MaintenanceWindowTaskParameterValueExpression) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens._Coerce)

instance
  Core.FromJSON
    MaintenanceWindowTaskParameterValueExpression
  where
  parseJSON =
    Core.withObject
      "MaintenanceWindowTaskParameterValueExpression"
      ( \x ->
          MaintenanceWindowTaskParameterValueExpression'
            Prelude.<$> (x Core..:? "Values" Core..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    MaintenanceWindowTaskParameterValueExpression

instance
  Prelude.NFData
    MaintenanceWindowTaskParameterValueExpression

instance
  Core.ToJSON
    MaintenanceWindowTaskParameterValueExpression
  where
  toJSON
    MaintenanceWindowTaskParameterValueExpression' {..} =
      Core.object
        ( Prelude.catMaybes
            [("Values" Core..=) Prelude.<$> values]
        )
