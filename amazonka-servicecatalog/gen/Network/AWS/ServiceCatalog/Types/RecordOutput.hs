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
-- Module      : Network.AWS.ServiceCatalog.Types.RecordOutput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.RecordOutput where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The output for the product created as the result of a request. For
-- example, the output for a CloudFormation-backed product that creates an
-- S3 bucket would include the S3 bucket URL.
--
-- /See:/ 'newRecordOutput' smart constructor.
data RecordOutput = RecordOutput'
  { -- | The output key.
    outputKey :: Core.Maybe Core.Text,
    -- | The output value.
    outputValue :: Core.Maybe Core.Text,
    -- | The description of the output.
    description :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RecordOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputKey', 'recordOutput_outputKey' - The output key.
--
-- 'outputValue', 'recordOutput_outputValue' - The output value.
--
-- 'description', 'recordOutput_description' - The description of the output.
newRecordOutput ::
  RecordOutput
newRecordOutput =
  RecordOutput'
    { outputKey = Core.Nothing,
      outputValue = Core.Nothing,
      description = Core.Nothing
    }

-- | The output key.
recordOutput_outputKey :: Lens.Lens' RecordOutput (Core.Maybe Core.Text)
recordOutput_outputKey = Lens.lens (\RecordOutput' {outputKey} -> outputKey) (\s@RecordOutput' {} a -> s {outputKey = a} :: RecordOutput)

-- | The output value.
recordOutput_outputValue :: Lens.Lens' RecordOutput (Core.Maybe Core.Text)
recordOutput_outputValue = Lens.lens (\RecordOutput' {outputValue} -> outputValue) (\s@RecordOutput' {} a -> s {outputValue = a} :: RecordOutput)

-- | The description of the output.
recordOutput_description :: Lens.Lens' RecordOutput (Core.Maybe Core.Text)
recordOutput_description = Lens.lens (\RecordOutput' {description} -> description) (\s@RecordOutput' {} a -> s {description = a} :: RecordOutput)

instance Core.FromJSON RecordOutput where
  parseJSON =
    Core.withObject
      "RecordOutput"
      ( \x ->
          RecordOutput'
            Core.<$> (x Core..:? "OutputKey")
            Core.<*> (x Core..:? "OutputValue")
            Core.<*> (x Core..:? "Description")
      )

instance Core.Hashable RecordOutput

instance Core.NFData RecordOutput
