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
-- Module      : Amazonka.LookoutEquipment.Types.DatasetSchema
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.DatasetSchema where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the data schema used with the given dataset.
--
-- /See:/ 'newDatasetSchema' smart constructor.
data DatasetSchema = DatasetSchema'
  { inlineDataSchema :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetSchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inlineDataSchema', 'datasetSchema_inlineDataSchema' -
newDatasetSchema ::
  DatasetSchema
newDatasetSchema =
  DatasetSchema' {inlineDataSchema = Prelude.Nothing}

-- |
datasetSchema_inlineDataSchema :: Lens.Lens' DatasetSchema (Prelude.Maybe Prelude.Text)
datasetSchema_inlineDataSchema = Lens.lens (\DatasetSchema' {inlineDataSchema} -> inlineDataSchema) (\s@DatasetSchema' {} a -> s {inlineDataSchema = a} :: DatasetSchema)

instance Prelude.Hashable DatasetSchema where
  hashWithSalt _salt DatasetSchema' {..} =
    _salt `Prelude.hashWithSalt` inlineDataSchema

instance Prelude.NFData DatasetSchema where
  rnf DatasetSchema' {..} = Prelude.rnf inlineDataSchema

instance Data.ToJSON DatasetSchema where
  toJSON DatasetSchema' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InlineDataSchema" Data..=)
              Prelude.<$> inlineDataSchema
          ]
      )
