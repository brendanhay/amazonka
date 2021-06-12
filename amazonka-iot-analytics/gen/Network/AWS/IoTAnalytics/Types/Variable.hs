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
-- Module      : Network.AWS.IoTAnalytics.Types.Variable
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.Variable where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types.DatasetContentVersionValue
import Network.AWS.IoTAnalytics.Types.OutputFileUriValue
import qualified Network.AWS.Lens as Lens

-- | An instance of a variable to be passed to the @containerAction@
-- execution. Each variable must have a name and a value given by one of
-- @stringValue@, @datasetContentVersionValue@, or @outputFileUriValue@.
--
-- /See:/ 'newVariable' smart constructor.
data Variable = Variable'
  { -- | The value of the variable as a structure that specifies an output file
    -- URI.
    outputFileUriValue :: Core.Maybe OutputFileUriValue,
    -- | The value of the variable as a double (numeric).
    doubleValue :: Core.Maybe Core.Double,
    -- | The value of the variable as a string.
    stringValue :: Core.Maybe Core.Text,
    -- | The value of the variable as a structure that specifies a dataset
    -- content version.
    datasetContentVersionValue :: Core.Maybe DatasetContentVersionValue,
    -- | The name of the variable.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Variable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputFileUriValue', 'variable_outputFileUriValue' - The value of the variable as a structure that specifies an output file
-- URI.
--
-- 'doubleValue', 'variable_doubleValue' - The value of the variable as a double (numeric).
--
-- 'stringValue', 'variable_stringValue' - The value of the variable as a string.
--
-- 'datasetContentVersionValue', 'variable_datasetContentVersionValue' - The value of the variable as a structure that specifies a dataset
-- content version.
--
-- 'name', 'variable_name' - The name of the variable.
newVariable ::
  -- | 'name'
  Core.Text ->
  Variable
newVariable pName_ =
  Variable'
    { outputFileUriValue = Core.Nothing,
      doubleValue = Core.Nothing,
      stringValue = Core.Nothing,
      datasetContentVersionValue = Core.Nothing,
      name = pName_
    }

-- | The value of the variable as a structure that specifies an output file
-- URI.
variable_outputFileUriValue :: Lens.Lens' Variable (Core.Maybe OutputFileUriValue)
variable_outputFileUriValue = Lens.lens (\Variable' {outputFileUriValue} -> outputFileUriValue) (\s@Variable' {} a -> s {outputFileUriValue = a} :: Variable)

-- | The value of the variable as a double (numeric).
variable_doubleValue :: Lens.Lens' Variable (Core.Maybe Core.Double)
variable_doubleValue = Lens.lens (\Variable' {doubleValue} -> doubleValue) (\s@Variable' {} a -> s {doubleValue = a} :: Variable)

-- | The value of the variable as a string.
variable_stringValue :: Lens.Lens' Variable (Core.Maybe Core.Text)
variable_stringValue = Lens.lens (\Variable' {stringValue} -> stringValue) (\s@Variable' {} a -> s {stringValue = a} :: Variable)

-- | The value of the variable as a structure that specifies a dataset
-- content version.
variable_datasetContentVersionValue :: Lens.Lens' Variable (Core.Maybe DatasetContentVersionValue)
variable_datasetContentVersionValue = Lens.lens (\Variable' {datasetContentVersionValue} -> datasetContentVersionValue) (\s@Variable' {} a -> s {datasetContentVersionValue = a} :: Variable)

-- | The name of the variable.
variable_name :: Lens.Lens' Variable Core.Text
variable_name = Lens.lens (\Variable' {name} -> name) (\s@Variable' {} a -> s {name = a} :: Variable)

instance Core.FromJSON Variable where
  parseJSON =
    Core.withObject
      "Variable"
      ( \x ->
          Variable'
            Core.<$> (x Core..:? "outputFileUriValue")
            Core.<*> (x Core..:? "doubleValue")
            Core.<*> (x Core..:? "stringValue")
            Core.<*> (x Core..:? "datasetContentVersionValue")
            Core.<*> (x Core..: "name")
      )

instance Core.Hashable Variable

instance Core.NFData Variable

instance Core.ToJSON Variable where
  toJSON Variable' {..} =
    Core.object
      ( Core.catMaybes
          [ ("outputFileUriValue" Core..=)
              Core.<$> outputFileUriValue,
            ("doubleValue" Core..=) Core.<$> doubleValue,
            ("stringValue" Core..=) Core.<$> stringValue,
            ("datasetContentVersionValue" Core..=)
              Core.<$> datasetContentVersionValue,
            Core.Just ("name" Core..= name)
          ]
      )
