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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.ControlInputParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of parameters for a control. A control can have zero, one, or
-- more than one parameter. An example of a control with two parameters is:
-- \"backup plan frequency is at least @daily@ and the retention period is
-- at least @1 year@\". The first parameter is @daily@. The second
-- parameter is @1 year@.
--
-- /See:/ 'newControlInputParameter' smart constructor.
data ControlInputParameter = ControlInputParameter'
  { -- | The name of a parameter, for example, @BackupPlanFrequency@.
    parameterName :: Prelude.Maybe Prelude.Text,
    -- | The value of parameter, for example, @hourly@.
    parameterValue :: Prelude.Maybe Prelude.Text
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
-- 'parameterName', 'controlInputParameter_parameterName' - The name of a parameter, for example, @BackupPlanFrequency@.
--
-- 'parameterValue', 'controlInputParameter_parameterValue' - The value of parameter, for example, @hourly@.
newControlInputParameter ::
  ControlInputParameter
newControlInputParameter =
  ControlInputParameter'
    { parameterName =
        Prelude.Nothing,
      parameterValue = Prelude.Nothing
    }

-- | The name of a parameter, for example, @BackupPlanFrequency@.
controlInputParameter_parameterName :: Lens.Lens' ControlInputParameter (Prelude.Maybe Prelude.Text)
controlInputParameter_parameterName = Lens.lens (\ControlInputParameter' {parameterName} -> parameterName) (\s@ControlInputParameter' {} a -> s {parameterName = a} :: ControlInputParameter)

-- | The value of parameter, for example, @hourly@.
controlInputParameter_parameterValue :: Lens.Lens' ControlInputParameter (Prelude.Maybe Prelude.Text)
controlInputParameter_parameterValue = Lens.lens (\ControlInputParameter' {parameterValue} -> parameterValue) (\s@ControlInputParameter' {} a -> s {parameterValue = a} :: ControlInputParameter)

instance Data.FromJSON ControlInputParameter where
  parseJSON =
    Data.withObject
      "ControlInputParameter"
      ( \x ->
          ControlInputParameter'
            Prelude.<$> (x Data..:? "ParameterName")
            Prelude.<*> (x Data..:? "ParameterValue")
      )

instance Prelude.Hashable ControlInputParameter where
  hashWithSalt _salt ControlInputParameter' {..} =
    _salt
      `Prelude.hashWithSalt` parameterName
      `Prelude.hashWithSalt` parameterValue

instance Prelude.NFData ControlInputParameter where
  rnf ControlInputParameter' {..} =
    Prelude.rnf parameterName `Prelude.seq`
      Prelude.rnf parameterValue

instance Data.ToJSON ControlInputParameter where
  toJSON ControlInputParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ParameterName" Data..=) Prelude.<$> parameterName,
            ("ParameterValue" Data..=)
              Prelude.<$> parameterValue
          ]
      )
