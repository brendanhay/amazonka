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
-- Module      : Amazonka.Glue.Types.AmazonRedshiftSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.AmazonRedshiftSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.AmazonRedshiftNodeData
import qualified Amazonka.Prelude as Prelude

-- | Specifies an Amazon Redshift source.
--
-- /See:/ 'newAmazonRedshiftSource' smart constructor.
data AmazonRedshiftSource = AmazonRedshiftSource'
  { -- | Specifies the data of the Amazon Reshift source node.
    data' :: Prelude.Maybe AmazonRedshiftNodeData,
    -- | The name of the Amazon Redshift source.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AmazonRedshiftSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'data'', 'amazonRedshiftSource_data' - Specifies the data of the Amazon Reshift source node.
--
-- 'name', 'amazonRedshiftSource_name' - The name of the Amazon Redshift source.
newAmazonRedshiftSource ::
  AmazonRedshiftSource
newAmazonRedshiftSource =
  AmazonRedshiftSource'
    { data' = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | Specifies the data of the Amazon Reshift source node.
amazonRedshiftSource_data :: Lens.Lens' AmazonRedshiftSource (Prelude.Maybe AmazonRedshiftNodeData)
amazonRedshiftSource_data = Lens.lens (\AmazonRedshiftSource' {data'} -> data') (\s@AmazonRedshiftSource' {} a -> s {data' = a} :: AmazonRedshiftSource)

-- | The name of the Amazon Redshift source.
amazonRedshiftSource_name :: Lens.Lens' AmazonRedshiftSource (Prelude.Maybe Prelude.Text)
amazonRedshiftSource_name = Lens.lens (\AmazonRedshiftSource' {name} -> name) (\s@AmazonRedshiftSource' {} a -> s {name = a} :: AmazonRedshiftSource)

instance Data.FromJSON AmazonRedshiftSource where
  parseJSON =
    Data.withObject
      "AmazonRedshiftSource"
      ( \x ->
          AmazonRedshiftSource'
            Prelude.<$> (x Data..:? "Data")
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable AmazonRedshiftSource where
  hashWithSalt _salt AmazonRedshiftSource' {..} =
    _salt
      `Prelude.hashWithSalt` data'
      `Prelude.hashWithSalt` name

instance Prelude.NFData AmazonRedshiftSource where
  rnf AmazonRedshiftSource' {..} =
    Prelude.rnf data' `Prelude.seq` Prelude.rnf name

instance Data.ToJSON AmazonRedshiftSource where
  toJSON AmazonRedshiftSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Data" Data..=) Prelude.<$> data',
            ("Name" Data..=) Prelude.<$> name
          ]
      )
