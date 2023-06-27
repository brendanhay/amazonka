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
-- Module      : Amazonka.ServiceCatalog.Types.RecordOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.RecordOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The output for the product created as the result of a request. For
-- example, the output for a CloudFormation-backed product that creates an
-- S3 bucket would include the S3 bucket URL.
--
-- /See:/ 'newRecordOutput' smart constructor.
data RecordOutput = RecordOutput'
  { -- | The description of the output.
    description :: Prelude.Maybe Prelude.Text,
    -- | The output key.
    outputKey :: Prelude.Maybe Prelude.Text,
    -- | The output value.
    outputValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecordOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'recordOutput_description' - The description of the output.
--
-- 'outputKey', 'recordOutput_outputKey' - The output key.
--
-- 'outputValue', 'recordOutput_outputValue' - The output value.
newRecordOutput ::
  RecordOutput
newRecordOutput =
  RecordOutput'
    { description = Prelude.Nothing,
      outputKey = Prelude.Nothing,
      outputValue = Prelude.Nothing
    }

-- | The description of the output.
recordOutput_description :: Lens.Lens' RecordOutput (Prelude.Maybe Prelude.Text)
recordOutput_description = Lens.lens (\RecordOutput' {description} -> description) (\s@RecordOutput' {} a -> s {description = a} :: RecordOutput)

-- | The output key.
recordOutput_outputKey :: Lens.Lens' RecordOutput (Prelude.Maybe Prelude.Text)
recordOutput_outputKey = Lens.lens (\RecordOutput' {outputKey} -> outputKey) (\s@RecordOutput' {} a -> s {outputKey = a} :: RecordOutput)

-- | The output value.
recordOutput_outputValue :: Lens.Lens' RecordOutput (Prelude.Maybe Prelude.Text)
recordOutput_outputValue = Lens.lens (\RecordOutput' {outputValue} -> outputValue) (\s@RecordOutput' {} a -> s {outputValue = a} :: RecordOutput)

instance Data.FromJSON RecordOutput where
  parseJSON =
    Data.withObject
      "RecordOutput"
      ( \x ->
          RecordOutput'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "OutputKey")
            Prelude.<*> (x Data..:? "OutputValue")
      )

instance Prelude.Hashable RecordOutput where
  hashWithSalt _salt RecordOutput' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` outputKey
      `Prelude.hashWithSalt` outputValue

instance Prelude.NFData RecordOutput where
  rnf RecordOutput' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf outputKey
      `Prelude.seq` Prelude.rnf outputValue

instance Data.ToJSON RecordOutput where
  toJSON RecordOutput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("OutputKey" Data..=) Prelude.<$> outputKey,
            ("OutputValue" Data..=) Prelude.<$> outputValue
          ]
      )
