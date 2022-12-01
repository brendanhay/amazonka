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
-- Module      : Amazonka.MigrationHubStrategy.Types.DatabaseConfigDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.DatabaseConfigDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Configuration information used for assessing databases.
--
-- /See:/ 'newDatabaseConfigDetail' smart constructor.
data DatabaseConfigDetail = DatabaseConfigDetail'
  { -- | AWS Secrets Manager key that holds the credentials that you use to
    -- connect to a database.
    secretName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatabaseConfigDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secretName', 'databaseConfigDetail_secretName' - AWS Secrets Manager key that holds the credentials that you use to
-- connect to a database.
newDatabaseConfigDetail ::
  DatabaseConfigDetail
newDatabaseConfigDetail =
  DatabaseConfigDetail' {secretName = Prelude.Nothing}

-- | AWS Secrets Manager key that holds the credentials that you use to
-- connect to a database.
databaseConfigDetail_secretName :: Lens.Lens' DatabaseConfigDetail (Prelude.Maybe Prelude.Text)
databaseConfigDetail_secretName = Lens.lens (\DatabaseConfigDetail' {secretName} -> secretName) (\s@DatabaseConfigDetail' {} a -> s {secretName = a} :: DatabaseConfigDetail)

instance Core.FromJSON DatabaseConfigDetail where
  parseJSON =
    Core.withObject
      "DatabaseConfigDetail"
      ( \x ->
          DatabaseConfigDetail'
            Prelude.<$> (x Core..:? "secretName")
      )

instance Prelude.Hashable DatabaseConfigDetail where
  hashWithSalt _salt DatabaseConfigDetail' {..} =
    _salt `Prelude.hashWithSalt` secretName

instance Prelude.NFData DatabaseConfigDetail where
  rnf DatabaseConfigDetail' {..} =
    Prelude.rnf secretName
