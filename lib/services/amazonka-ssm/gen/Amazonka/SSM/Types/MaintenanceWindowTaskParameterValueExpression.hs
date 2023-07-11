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
-- Module      : Amazonka.SSM.Types.MaintenanceWindowTaskParameterValueExpression
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.MaintenanceWindowTaskParameterValueExpression where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines the values for a task parameter.
--
-- /See:/ 'newMaintenanceWindowTaskParameterValueExpression' smart constructor.
data MaintenanceWindowTaskParameterValueExpression = MaintenanceWindowTaskParameterValueExpression'
  { -- | This field contains an array of 0 or more strings, each 1 to 255
    -- characters in length.
    values :: Prelude.Maybe (Data.Sensitive [Data.Sensitive Prelude.Text])
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
maintenanceWindowTaskParameterValueExpression_values = Lens.lens (\MaintenanceWindowTaskParameterValueExpression' {values} -> values) (\s@MaintenanceWindowTaskParameterValueExpression' {} a -> s {values = a} :: MaintenanceWindowTaskParameterValueExpression) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

instance
  Data.FromJSON
    MaintenanceWindowTaskParameterValueExpression
  where
  parseJSON =
    Data.withObject
      "MaintenanceWindowTaskParameterValueExpression"
      ( \x ->
          MaintenanceWindowTaskParameterValueExpression'
            Prelude.<$> (x Data..:? "Values" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    MaintenanceWindowTaskParameterValueExpression
  where
  hashWithSalt
    _salt
    MaintenanceWindowTaskParameterValueExpression' {..} =
      _salt `Prelude.hashWithSalt` values

instance
  Prelude.NFData
    MaintenanceWindowTaskParameterValueExpression
  where
  rnf
    MaintenanceWindowTaskParameterValueExpression' {..} =
      Prelude.rnf values

instance
  Data.ToJSON
    MaintenanceWindowTaskParameterValueExpression
  where
  toJSON
    MaintenanceWindowTaskParameterValueExpression' {..} =
      Data.object
        ( Prelude.catMaybes
            [("Values" Data..=) Prelude.<$> values]
        )
