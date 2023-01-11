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
-- Module      : Amazonka.IoTAnalytics.Types.Column
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.Column where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a column that stores your data.
--
-- /See:/ 'newColumn' smart constructor.
data Column = Column'
  { -- | The name of the column.
    name :: Prelude.Text,
    -- | The type of data. For more information about the supported data types,
    -- see
    -- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-api-common.html Common data types>
    -- in the /Glue Developer Guide/.
    type' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Column' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'column_name' - The name of the column.
--
-- 'type'', 'column_type' - The type of data. For more information about the supported data types,
-- see
-- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-api-common.html Common data types>
-- in the /Glue Developer Guide/.
newColumn ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  Prelude.Text ->
  Column
newColumn pName_ pType_ =
  Column' {name = pName_, type' = pType_}

-- | The name of the column.
column_name :: Lens.Lens' Column Prelude.Text
column_name = Lens.lens (\Column' {name} -> name) (\s@Column' {} a -> s {name = a} :: Column)

-- | The type of data. For more information about the supported data types,
-- see
-- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-api-common.html Common data types>
-- in the /Glue Developer Guide/.
column_type :: Lens.Lens' Column Prelude.Text
column_type = Lens.lens (\Column' {type'} -> type') (\s@Column' {} a -> s {type' = a} :: Column)

instance Data.FromJSON Column where
  parseJSON =
    Data.withObject
      "Column"
      ( \x ->
          Column'
            Prelude.<$> (x Data..: "name") Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable Column where
  hashWithSalt _salt Column' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Column where
  rnf Column' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON Column where
  toJSON Column' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Data..= name),
            Prelude.Just ("type" Data..= type')
          ]
      )
