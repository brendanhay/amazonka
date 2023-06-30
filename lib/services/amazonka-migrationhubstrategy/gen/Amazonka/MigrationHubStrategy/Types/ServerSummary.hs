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
-- Module      : Amazonka.MigrationHubStrategy.Types.ServerSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.ServerSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.ServerOsType
import qualified Amazonka.Prelude as Prelude

-- | Object containing details about the servers imported by Application
-- Discovery Service
--
-- /See:/ 'newServerSummary' smart constructor.
data ServerSummary = ServerSummary'
  { -- | Type of operating system for the servers.
    serverOsType :: Prelude.Maybe ServerOsType,
    -- | Number of servers.
    count :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServerSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverOsType', 'serverSummary_serverOsType' - Type of operating system for the servers.
--
-- 'count', 'serverSummary_count' - Number of servers.
newServerSummary ::
  ServerSummary
newServerSummary =
  ServerSummary'
    { serverOsType = Prelude.Nothing,
      count = Prelude.Nothing
    }

-- | Type of operating system for the servers.
serverSummary_serverOsType :: Lens.Lens' ServerSummary (Prelude.Maybe ServerOsType)
serverSummary_serverOsType = Lens.lens (\ServerSummary' {serverOsType} -> serverOsType) (\s@ServerSummary' {} a -> s {serverOsType = a} :: ServerSummary)

-- | Number of servers.
serverSummary_count :: Lens.Lens' ServerSummary (Prelude.Maybe Prelude.Int)
serverSummary_count = Lens.lens (\ServerSummary' {count} -> count) (\s@ServerSummary' {} a -> s {count = a} :: ServerSummary)

instance Data.FromJSON ServerSummary where
  parseJSON =
    Data.withObject
      "ServerSummary"
      ( \x ->
          ServerSummary'
            Prelude.<$> (x Data..:? "ServerOsType")
            Prelude.<*> (x Data..:? "count")
      )

instance Prelude.Hashable ServerSummary where
  hashWithSalt _salt ServerSummary' {..} =
    _salt
      `Prelude.hashWithSalt` serverOsType
      `Prelude.hashWithSalt` count

instance Prelude.NFData ServerSummary where
  rnf ServerSummary' {..} =
    Prelude.rnf serverOsType
      `Prelude.seq` Prelude.rnf count
