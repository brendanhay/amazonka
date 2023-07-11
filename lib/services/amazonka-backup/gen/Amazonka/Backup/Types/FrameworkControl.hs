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
-- Module      : Amazonka.Backup.Types.FrameworkControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.FrameworkControl where

import Amazonka.Backup.Types.ControlInputParameter
import Amazonka.Backup.Types.ControlScope
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains detailed information about all of the controls of a framework.
-- Each framework must contain at least one control.
--
-- /See:/ 'newFrameworkControl' smart constructor.
data FrameworkControl = FrameworkControl'
  { -- | A list of @ParameterName@ and @ParameterValue@ pairs.
    controlInputParameters :: Prelude.Maybe [ControlInputParameter],
    -- | The scope of a control. The control scope defines what the control will
    -- evaluate. Three examples of control scopes are: a specific backup plan,
    -- all backup plans with a specific tag, or all backup plans. For more
    -- information, see
    -- <aws-backup/latest/devguide/API_ControlScope.html ControlScope.>
    controlScope :: Prelude.Maybe ControlScope,
    -- | The name of a control. This name is between 1 and 256 characters.
    controlName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FrameworkControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'controlInputParameters', 'frameworkControl_controlInputParameters' - A list of @ParameterName@ and @ParameterValue@ pairs.
--
-- 'controlScope', 'frameworkControl_controlScope' - The scope of a control. The control scope defines what the control will
-- evaluate. Three examples of control scopes are: a specific backup plan,
-- all backup plans with a specific tag, or all backup plans. For more
-- information, see
-- <aws-backup/latest/devguide/API_ControlScope.html ControlScope.>
--
-- 'controlName', 'frameworkControl_controlName' - The name of a control. This name is between 1 and 256 characters.
newFrameworkControl ::
  -- | 'controlName'
  Prelude.Text ->
  FrameworkControl
newFrameworkControl pControlName_ =
  FrameworkControl'
    { controlInputParameters =
        Prelude.Nothing,
      controlScope = Prelude.Nothing,
      controlName = pControlName_
    }

-- | A list of @ParameterName@ and @ParameterValue@ pairs.
frameworkControl_controlInputParameters :: Lens.Lens' FrameworkControl (Prelude.Maybe [ControlInputParameter])
frameworkControl_controlInputParameters = Lens.lens (\FrameworkControl' {controlInputParameters} -> controlInputParameters) (\s@FrameworkControl' {} a -> s {controlInputParameters = a} :: FrameworkControl) Prelude.. Lens.mapping Lens.coerced

-- | The scope of a control. The control scope defines what the control will
-- evaluate. Three examples of control scopes are: a specific backup plan,
-- all backup plans with a specific tag, or all backup plans. For more
-- information, see
-- <aws-backup/latest/devguide/API_ControlScope.html ControlScope.>
frameworkControl_controlScope :: Lens.Lens' FrameworkControl (Prelude.Maybe ControlScope)
frameworkControl_controlScope = Lens.lens (\FrameworkControl' {controlScope} -> controlScope) (\s@FrameworkControl' {} a -> s {controlScope = a} :: FrameworkControl)

-- | The name of a control. This name is between 1 and 256 characters.
frameworkControl_controlName :: Lens.Lens' FrameworkControl Prelude.Text
frameworkControl_controlName = Lens.lens (\FrameworkControl' {controlName} -> controlName) (\s@FrameworkControl' {} a -> s {controlName = a} :: FrameworkControl)

instance Data.FromJSON FrameworkControl where
  parseJSON =
    Data.withObject
      "FrameworkControl"
      ( \x ->
          FrameworkControl'
            Prelude.<$> ( x
                            Data..:? "ControlInputParameters"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ControlScope")
            Prelude.<*> (x Data..: "ControlName")
      )

instance Prelude.Hashable FrameworkControl where
  hashWithSalt _salt FrameworkControl' {..} =
    _salt
      `Prelude.hashWithSalt` controlInputParameters
      `Prelude.hashWithSalt` controlScope
      `Prelude.hashWithSalt` controlName

instance Prelude.NFData FrameworkControl where
  rnf FrameworkControl' {..} =
    Prelude.rnf controlInputParameters
      `Prelude.seq` Prelude.rnf controlScope
      `Prelude.seq` Prelude.rnf controlName

instance Data.ToJSON FrameworkControl where
  toJSON FrameworkControl' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ControlInputParameters" Data..=)
              Prelude.<$> controlInputParameters,
            ("ControlScope" Data..=) Prelude.<$> controlScope,
            Prelude.Just ("ControlName" Data..= controlName)
          ]
      )
