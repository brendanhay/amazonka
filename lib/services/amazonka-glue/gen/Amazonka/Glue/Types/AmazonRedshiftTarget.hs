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
-- Module      : Amazonka.Glue.Types.AmazonRedshiftTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.AmazonRedshiftTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.AmazonRedshiftNodeData
import qualified Amazonka.Prelude as Prelude

-- | Specifies an Amazon Redshift target.
--
-- /See:/ 'newAmazonRedshiftTarget' smart constructor.
data AmazonRedshiftTarget = AmazonRedshiftTarget'
  { -- | Specifies the data of the Amazon Reshift target node.
    data' :: Prelude.Maybe AmazonRedshiftNodeData,
    -- | The nodes that are inputs to the data target.
    inputs :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The name of the Amazon Redshift target.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AmazonRedshiftTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'data'', 'amazonRedshiftTarget_data' - Specifies the data of the Amazon Reshift target node.
--
-- 'inputs', 'amazonRedshiftTarget_inputs' - The nodes that are inputs to the data target.
--
-- 'name', 'amazonRedshiftTarget_name' - The name of the Amazon Redshift target.
newAmazonRedshiftTarget ::
  AmazonRedshiftTarget
newAmazonRedshiftTarget =
  AmazonRedshiftTarget'
    { data' = Prelude.Nothing,
      inputs = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | Specifies the data of the Amazon Reshift target node.
amazonRedshiftTarget_data :: Lens.Lens' AmazonRedshiftTarget (Prelude.Maybe AmazonRedshiftNodeData)
amazonRedshiftTarget_data = Lens.lens (\AmazonRedshiftTarget' {data'} -> data') (\s@AmazonRedshiftTarget' {} a -> s {data' = a} :: AmazonRedshiftTarget)

-- | The nodes that are inputs to the data target.
amazonRedshiftTarget_inputs :: Lens.Lens' AmazonRedshiftTarget (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
amazonRedshiftTarget_inputs = Lens.lens (\AmazonRedshiftTarget' {inputs} -> inputs) (\s@AmazonRedshiftTarget' {} a -> s {inputs = a} :: AmazonRedshiftTarget) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Amazon Redshift target.
amazonRedshiftTarget_name :: Lens.Lens' AmazonRedshiftTarget (Prelude.Maybe Prelude.Text)
amazonRedshiftTarget_name = Lens.lens (\AmazonRedshiftTarget' {name} -> name) (\s@AmazonRedshiftTarget' {} a -> s {name = a} :: AmazonRedshiftTarget)

instance Data.FromJSON AmazonRedshiftTarget where
  parseJSON =
    Data.withObject
      "AmazonRedshiftTarget"
      ( \x ->
          AmazonRedshiftTarget'
            Prelude.<$> (x Data..:? "Data")
            Prelude.<*> (x Data..:? "Inputs")
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable AmazonRedshiftTarget where
  hashWithSalt _salt AmazonRedshiftTarget' {..} =
    _salt
      `Prelude.hashWithSalt` data'
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` name

instance Prelude.NFData AmazonRedshiftTarget where
  rnf AmazonRedshiftTarget' {..} =
    Prelude.rnf data'
      `Prelude.seq` Prelude.rnf inputs
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON AmazonRedshiftTarget where
  toJSON AmazonRedshiftTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Data" Data..=) Prelude.<$> data',
            ("Inputs" Data..=) Prelude.<$> inputs,
            ("Name" Data..=) Prelude.<$> name
          ]
      )
