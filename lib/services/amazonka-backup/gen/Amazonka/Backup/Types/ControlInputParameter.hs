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
-- Module      : Amazonka.Backup.Types.ControlInputParameter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.ControlInputParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A list of parameters for a control. A control can have zero, one, or
-- more than one parameter. An example of a control with two parameters is:
-- \"backup plan frequency is at least @daily@ and the retention period is
-- at least @1 year@\". The first parameter is @daily@. The second
-- parameter is @1 year@.
--
-- /See:/ 'newControlInputParameter' smart constructor.
data ControlInputParameter = ControlInputParameter'
  { -- | The value of parameter, for example, @hourly@.
    parameterValue :: Prelude.Maybe Prelude.Text,
    -- | The name of a parameter, for example, @BackupPlanFrequency@.
    parameterName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ControlInputParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterValue', 'controlInputParameter_parameterValue' - The value of parameter, for example, @hourly@.
--
-- 'parameterName', 'controlInputParameter_parameterName' - The name of a parameter, for example, @BackupPlanFrequency@.
newControlInputParameter ::
  ControlInputParameter
newControlInputParameter =
  ControlInputParameter'
    { parameterValue =
        Prelude.Nothing,
      parameterName = Prelude.Nothing
    }

-- | The value of parameter, for example, @hourly@.
controlInputParameter_parameterValue :: Lens.Lens' ControlInputParameter (Prelude.Maybe Prelude.Text)
controlInputParameter_parameterValue = Lens.lens (\ControlInputParameter' {parameterValue} -> parameterValue) (\s@ControlInputParameter' {} a -> s {parameterValue = a} :: ControlInputParameter)

-- | The name of a parameter, for example, @BackupPlanFrequency@.
controlInputParameter_parameterName :: Lens.Lens' ControlInputParameter (Prelude.Maybe Prelude.Text)
controlInputParameter_parameterName = Lens.lens (\ControlInputParameter' {parameterName} -> parameterName) (\s@ControlInputParameter' {} a -> s {parameterName = a} :: ControlInputParameter)

instance Core.FromJSON ControlInputParameter where
  parseJSON =
    Core.withObject
      "ControlInputParameter"
      ( \x ->
          ControlInputParameter'
            Prelude.<$> (x Core..:? "ParameterValue")
            Prelude.<*> (x Core..:? "ParameterName")
      )

instance Prelude.Hashable ControlInputParameter where
  hashWithSalt _salt ControlInputParameter' {..} =
    _salt `Prelude.hashWithSalt` parameterValue
      `Prelude.hashWithSalt` parameterName

instance Prelude.NFData ControlInputParameter where
  rnf ControlInputParameter' {..} =
    Prelude.rnf parameterValue
      `Prelude.seq` Prelude.rnf parameterName

instance Core.ToJSON ControlInputParameter where
  toJSON ControlInputParameter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ParameterValue" Core..=)
              Prelude.<$> parameterValue,
            ("ParameterName" Core..=) Prelude.<$> parameterName
          ]
      )
