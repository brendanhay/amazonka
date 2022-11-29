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
-- Module      : Amazonka.GuardDuty.Types.UnprocessedDataSourcesResult
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.UnprocessedDataSourcesResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GuardDuty.Types.MalwareProtectionConfigurationResult
import qualified Amazonka.Prelude as Prelude

-- | Specifies the names of the data sources that couldn\'t be enabled.
--
-- /See:/ 'newUnprocessedDataSourcesResult' smart constructor.
data UnprocessedDataSourcesResult = UnprocessedDataSourcesResult'
  { malwareProtection :: Prelude.Maybe MalwareProtectionConfigurationResult
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnprocessedDataSourcesResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'malwareProtection', 'unprocessedDataSourcesResult_malwareProtection' - Undocumented member.
newUnprocessedDataSourcesResult ::
  UnprocessedDataSourcesResult
newUnprocessedDataSourcesResult =
  UnprocessedDataSourcesResult'
    { malwareProtection =
        Prelude.Nothing
    }

-- | Undocumented member.
unprocessedDataSourcesResult_malwareProtection :: Lens.Lens' UnprocessedDataSourcesResult (Prelude.Maybe MalwareProtectionConfigurationResult)
unprocessedDataSourcesResult_malwareProtection = Lens.lens (\UnprocessedDataSourcesResult' {malwareProtection} -> malwareProtection) (\s@UnprocessedDataSourcesResult' {} a -> s {malwareProtection = a} :: UnprocessedDataSourcesResult)

instance Core.FromJSON UnprocessedDataSourcesResult where
  parseJSON =
    Core.withObject
      "UnprocessedDataSourcesResult"
      ( \x ->
          UnprocessedDataSourcesResult'
            Prelude.<$> (x Core..:? "malwareProtection")
      )

instance
  Prelude.Hashable
    UnprocessedDataSourcesResult
  where
  hashWithSalt _salt UnprocessedDataSourcesResult' {..} =
    _salt `Prelude.hashWithSalt` malwareProtection

instance Prelude.NFData UnprocessedDataSourcesResult where
  rnf UnprocessedDataSourcesResult' {..} =
    Prelude.rnf malwareProtection
