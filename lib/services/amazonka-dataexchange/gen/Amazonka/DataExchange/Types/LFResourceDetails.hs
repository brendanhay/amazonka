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
-- Module      : Amazonka.DataExchange.Types.LFResourceDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.LFResourceDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types.DatabaseLFTagPolicy
import Amazonka.DataExchange.Types.TableLFTagPolicy
import qualified Amazonka.Prelude as Prelude

-- | Details about the AWS Lake Formation resource (Table or Database)
-- included in the AWS Lake Formation data permission.
--
-- /See:/ 'newLFResourceDetails' smart constructor.
data LFResourceDetails = LFResourceDetails'
  { -- | Details about the database resource included in the AWS Lake Formation
    -- data permission.
    database :: Prelude.Maybe DatabaseLFTagPolicy,
    -- | Details about the table resource included in the AWS Lake Formation data
    -- permission.
    table :: Prelude.Maybe TableLFTagPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LFResourceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'database', 'lFResourceDetails_database' - Details about the database resource included in the AWS Lake Formation
-- data permission.
--
-- 'table', 'lFResourceDetails_table' - Details about the table resource included in the AWS Lake Formation data
-- permission.
newLFResourceDetails ::
  LFResourceDetails
newLFResourceDetails =
  LFResourceDetails'
    { database = Prelude.Nothing,
      table = Prelude.Nothing
    }

-- | Details about the database resource included in the AWS Lake Formation
-- data permission.
lFResourceDetails_database :: Lens.Lens' LFResourceDetails (Prelude.Maybe DatabaseLFTagPolicy)
lFResourceDetails_database = Lens.lens (\LFResourceDetails' {database} -> database) (\s@LFResourceDetails' {} a -> s {database = a} :: LFResourceDetails)

-- | Details about the table resource included in the AWS Lake Formation data
-- permission.
lFResourceDetails_table :: Lens.Lens' LFResourceDetails (Prelude.Maybe TableLFTagPolicy)
lFResourceDetails_table = Lens.lens (\LFResourceDetails' {table} -> table) (\s@LFResourceDetails' {} a -> s {table = a} :: LFResourceDetails)

instance Data.FromJSON LFResourceDetails where
  parseJSON =
    Data.withObject
      "LFResourceDetails"
      ( \x ->
          LFResourceDetails'
            Prelude.<$> (x Data..:? "Database")
            Prelude.<*> (x Data..:? "Table")
      )

instance Prelude.Hashable LFResourceDetails where
  hashWithSalt _salt LFResourceDetails' {..} =
    _salt
      `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` table

instance Prelude.NFData LFResourceDetails where
  rnf LFResourceDetails' {..} =
    Prelude.rnf database `Prelude.seq`
      Prelude.rnf table
