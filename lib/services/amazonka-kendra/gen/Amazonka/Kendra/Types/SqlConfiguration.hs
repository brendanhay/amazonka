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
-- Module      : Amazonka.Kendra.Types.SqlConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.SqlConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.QueryIdentifiersEnclosingOption
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information to use a SQL database.
--
-- /See:/ 'newSqlConfiguration' smart constructor.
data SqlConfiguration = SqlConfiguration'
  { -- | Determines whether Amazon Kendra encloses SQL identifiers for tables and
    -- column names in double quotes (\") when making a database query.
    --
    -- By default, Amazon Kendra passes SQL identifiers the way that they are
    -- entered into the data source configuration. It does not change the case
    -- of identifiers or enclose them in quotes.
    --
    -- PostgreSQL internally converts uppercase characters to lower case
    -- characters in identifiers unless they are quoted. Choosing this option
    -- encloses identifiers in quotes so that PostgreSQL does not convert the
    -- character\'s case.
    --
    -- For MySQL databases, you must enable the @ansi_quotes@ option when you
    -- set this field to @DOUBLE_QUOTES@.
    queryIdentifiersEnclosingOption :: Prelude.Maybe QueryIdentifiersEnclosingOption
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SqlConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryIdentifiersEnclosingOption', 'sqlConfiguration_queryIdentifiersEnclosingOption' - Determines whether Amazon Kendra encloses SQL identifiers for tables and
-- column names in double quotes (\") when making a database query.
--
-- By default, Amazon Kendra passes SQL identifiers the way that they are
-- entered into the data source configuration. It does not change the case
-- of identifiers or enclose them in quotes.
--
-- PostgreSQL internally converts uppercase characters to lower case
-- characters in identifiers unless they are quoted. Choosing this option
-- encloses identifiers in quotes so that PostgreSQL does not convert the
-- character\'s case.
--
-- For MySQL databases, you must enable the @ansi_quotes@ option when you
-- set this field to @DOUBLE_QUOTES@.
newSqlConfiguration ::
  SqlConfiguration
newSqlConfiguration =
  SqlConfiguration'
    { queryIdentifiersEnclosingOption =
        Prelude.Nothing
    }

-- | Determines whether Amazon Kendra encloses SQL identifiers for tables and
-- column names in double quotes (\") when making a database query.
--
-- By default, Amazon Kendra passes SQL identifiers the way that they are
-- entered into the data source configuration. It does not change the case
-- of identifiers or enclose them in quotes.
--
-- PostgreSQL internally converts uppercase characters to lower case
-- characters in identifiers unless they are quoted. Choosing this option
-- encloses identifiers in quotes so that PostgreSQL does not convert the
-- character\'s case.
--
-- For MySQL databases, you must enable the @ansi_quotes@ option when you
-- set this field to @DOUBLE_QUOTES@.
sqlConfiguration_queryIdentifiersEnclosingOption :: Lens.Lens' SqlConfiguration (Prelude.Maybe QueryIdentifiersEnclosingOption)
sqlConfiguration_queryIdentifiersEnclosingOption = Lens.lens (\SqlConfiguration' {queryIdentifiersEnclosingOption} -> queryIdentifiersEnclosingOption) (\s@SqlConfiguration' {} a -> s {queryIdentifiersEnclosingOption = a} :: SqlConfiguration)

instance Data.FromJSON SqlConfiguration where
  parseJSON =
    Data.withObject
      "SqlConfiguration"
      ( \x ->
          SqlConfiguration'
            Prelude.<$> (x Data..:? "QueryIdentifiersEnclosingOption")
      )

instance Prelude.Hashable SqlConfiguration where
  hashWithSalt _salt SqlConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` queryIdentifiersEnclosingOption

instance Prelude.NFData SqlConfiguration where
  rnf SqlConfiguration' {..} =
    Prelude.rnf queryIdentifiersEnclosingOption

instance Data.ToJSON SqlConfiguration where
  toJSON SqlConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("QueryIdentifiersEnclosingOption" Data..=)
              Prelude.<$> queryIdentifiersEnclosingOption
          ]
      )
