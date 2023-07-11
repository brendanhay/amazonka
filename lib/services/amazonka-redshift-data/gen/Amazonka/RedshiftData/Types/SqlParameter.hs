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
-- Module      : Amazonka.RedshiftData.Types.SqlParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RedshiftData.Types.SqlParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A parameter used in a SQL statement.
--
-- /See:/ 'newSqlParameter' smart constructor.
data SqlParameter = SqlParameter'
  { -- | The name of the parameter.
    name :: Prelude.Text,
    -- | The value of the parameter. Amazon Redshift implicitly converts to the
    -- proper data type. For more information, see
    -- <https://docs.aws.amazon.com/redshift/latest/dg/c_Supported_data_types.html Data types>
    -- in the /Amazon Redshift Database Developer Guide/.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SqlParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'sqlParameter_name' - The name of the parameter.
--
-- 'value', 'sqlParameter_value' - The value of the parameter. Amazon Redshift implicitly converts to the
-- proper data type. For more information, see
-- <https://docs.aws.amazon.com/redshift/latest/dg/c_Supported_data_types.html Data types>
-- in the /Amazon Redshift Database Developer Guide/.
newSqlParameter ::
  -- | 'name'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  SqlParameter
newSqlParameter pName_ pValue_ =
  SqlParameter' {name = pName_, value = pValue_}

-- | The name of the parameter.
sqlParameter_name :: Lens.Lens' SqlParameter Prelude.Text
sqlParameter_name = Lens.lens (\SqlParameter' {name} -> name) (\s@SqlParameter' {} a -> s {name = a} :: SqlParameter)

-- | The value of the parameter. Amazon Redshift implicitly converts to the
-- proper data type. For more information, see
-- <https://docs.aws.amazon.com/redshift/latest/dg/c_Supported_data_types.html Data types>
-- in the /Amazon Redshift Database Developer Guide/.
sqlParameter_value :: Lens.Lens' SqlParameter Prelude.Text
sqlParameter_value = Lens.lens (\SqlParameter' {value} -> value) (\s@SqlParameter' {} a -> s {value = a} :: SqlParameter)

instance Data.FromJSON SqlParameter where
  parseJSON =
    Data.withObject
      "SqlParameter"
      ( \x ->
          SqlParameter'
            Prelude.<$> (x Data..: "name")
            Prelude.<*> (x Data..: "value")
      )

instance Prelude.Hashable SqlParameter where
  hashWithSalt _salt SqlParameter' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData SqlParameter where
  rnf SqlParameter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

instance Data.ToJSON SqlParameter where
  toJSON SqlParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Data..= name),
            Prelude.Just ("value" Data..= value)
          ]
      )
