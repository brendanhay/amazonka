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
-- Module      : Network.AWS.HoneyCode.Types.VariableValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.HoneyCode.Types.VariableValue where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The input variables to the app to be used by the InvokeScreenAutomation
-- action request.
--
-- /See:/ 'newVariableValue' smart constructor.
data VariableValue = VariableValue'
  { -- | Raw value of the variable.
    rawValue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VariableValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rawValue', 'variableValue_rawValue' - Raw value of the variable.
newVariableValue ::
  -- | 'rawValue'
  Prelude.Text ->
  VariableValue
newVariableValue pRawValue_ =
  VariableValue' {rawValue = pRawValue_}

-- | Raw value of the variable.
variableValue_rawValue :: Lens.Lens' VariableValue Prelude.Text
variableValue_rawValue = Lens.lens (\VariableValue' {rawValue} -> rawValue) (\s@VariableValue' {} a -> s {rawValue = a} :: VariableValue)

instance Prelude.Hashable VariableValue

instance Prelude.NFData VariableValue

instance Core.ToJSON VariableValue where
  toJSON VariableValue' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("rawValue" Core..= rawValue)]
      )
