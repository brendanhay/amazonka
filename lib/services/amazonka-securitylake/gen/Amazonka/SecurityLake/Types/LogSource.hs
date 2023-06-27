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
-- Module      : Amazonka.SecurityLake.Types.LogSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.LogSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityLake.Types.LogSourceResource

-- | Amazon Security Lake can collect logs and events from natively-supported
-- Amazon Web Services services and custom sources.
--
-- /See:/ 'newLogSource' smart constructor.
data LogSource = LogSource'
  { -- | Specify the account from which you want to collect logs.
    account :: Prelude.Maybe Prelude.Text,
    -- | Specify the Regions from which you want to collect logs.
    region :: Prelude.Maybe Prelude.Text,
    -- | Specify the sources from which you want to collect logs.
    sources :: Prelude.Maybe [LogSourceResource]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'account', 'logSource_account' - Specify the account from which you want to collect logs.
--
-- 'region', 'logSource_region' - Specify the Regions from which you want to collect logs.
--
-- 'sources', 'logSource_sources' - Specify the sources from which you want to collect logs.
newLogSource ::
  LogSource
newLogSource =
  LogSource'
    { account = Prelude.Nothing,
      region = Prelude.Nothing,
      sources = Prelude.Nothing
    }

-- | Specify the account from which you want to collect logs.
logSource_account :: Lens.Lens' LogSource (Prelude.Maybe Prelude.Text)
logSource_account = Lens.lens (\LogSource' {account} -> account) (\s@LogSource' {} a -> s {account = a} :: LogSource)

-- | Specify the Regions from which you want to collect logs.
logSource_region :: Lens.Lens' LogSource (Prelude.Maybe Prelude.Text)
logSource_region = Lens.lens (\LogSource' {region} -> region) (\s@LogSource' {} a -> s {region = a} :: LogSource)

-- | Specify the sources from which you want to collect logs.
logSource_sources :: Lens.Lens' LogSource (Prelude.Maybe [LogSourceResource])
logSource_sources = Lens.lens (\LogSource' {sources} -> sources) (\s@LogSource' {} a -> s {sources = a} :: LogSource) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON LogSource where
  parseJSON =
    Data.withObject
      "LogSource"
      ( \x ->
          LogSource'
            Prelude.<$> (x Data..:? "account")
            Prelude.<*> (x Data..:? "region")
            Prelude.<*> (x Data..:? "sources" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable LogSource where
  hashWithSalt _salt LogSource' {..} =
    _salt
      `Prelude.hashWithSalt` account
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` sources

instance Prelude.NFData LogSource where
  rnf LogSource' {..} =
    Prelude.rnf account
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf sources
