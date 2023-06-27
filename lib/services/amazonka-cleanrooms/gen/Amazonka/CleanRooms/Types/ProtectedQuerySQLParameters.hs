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
-- Module      : Amazonka.CleanRooms.Types.ProtectedQuerySQLParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.ProtectedQuerySQLParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The parameters for the SQL type Protected Query.
--
-- /See:/ 'newProtectedQuerySQLParameters' smart constructor.
data ProtectedQuerySQLParameters = ProtectedQuerySQLParameters'
  { -- | The query string to be submitted.
    queryString :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProtectedQuerySQLParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryString', 'protectedQuerySQLParameters_queryString' - The query string to be submitted.
newProtectedQuerySQLParameters ::
  -- | 'queryString'
  Prelude.Text ->
  ProtectedQuerySQLParameters
newProtectedQuerySQLParameters pQueryString_ =
  ProtectedQuerySQLParameters'
    { queryString =
        pQueryString_
    }

-- | The query string to be submitted.
protectedQuerySQLParameters_queryString :: Lens.Lens' ProtectedQuerySQLParameters Prelude.Text
protectedQuerySQLParameters_queryString = Lens.lens (\ProtectedQuerySQLParameters' {queryString} -> queryString) (\s@ProtectedQuerySQLParameters' {} a -> s {queryString = a} :: ProtectedQuerySQLParameters)

instance Data.FromJSON ProtectedQuerySQLParameters where
  parseJSON =
    Data.withObject
      "ProtectedQuerySQLParameters"
      ( \x ->
          ProtectedQuerySQLParameters'
            Prelude.<$> (x Data..: "queryString")
      )

instance Prelude.Hashable ProtectedQuerySQLParameters where
  hashWithSalt _salt ProtectedQuerySQLParameters' {..} =
    _salt `Prelude.hashWithSalt` queryString

instance Prelude.NFData ProtectedQuerySQLParameters where
  rnf ProtectedQuerySQLParameters' {..} =
    Prelude.rnf queryString

instance Data.ToJSON ProtectedQuerySQLParameters where
  toJSON ProtectedQuerySQLParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("queryString" Data..= queryString)]
      )
