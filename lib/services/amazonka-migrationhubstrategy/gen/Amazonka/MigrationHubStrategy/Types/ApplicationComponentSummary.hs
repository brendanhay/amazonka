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
-- Module      : Amazonka.MigrationHubStrategy.Types.ApplicationComponentSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.ApplicationComponentSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.AppType
import qualified Amazonka.Prelude as Prelude

-- | Contains the summary of application components.
--
-- /See:/ 'newApplicationComponentSummary' smart constructor.
data ApplicationComponentSummary = ApplicationComponentSummary'
  { -- | Contains the name of application types.
    appType :: Prelude.Maybe AppType,
    -- | Contains the count of application type.
    count :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationComponentSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appType', 'applicationComponentSummary_appType' - Contains the name of application types.
--
-- 'count', 'applicationComponentSummary_count' - Contains the count of application type.
newApplicationComponentSummary ::
  ApplicationComponentSummary
newApplicationComponentSummary =
  ApplicationComponentSummary'
    { appType =
        Prelude.Nothing,
      count = Prelude.Nothing
    }

-- | Contains the name of application types.
applicationComponentSummary_appType :: Lens.Lens' ApplicationComponentSummary (Prelude.Maybe AppType)
applicationComponentSummary_appType = Lens.lens (\ApplicationComponentSummary' {appType} -> appType) (\s@ApplicationComponentSummary' {} a -> s {appType = a} :: ApplicationComponentSummary)

-- | Contains the count of application type.
applicationComponentSummary_count :: Lens.Lens' ApplicationComponentSummary (Prelude.Maybe Prelude.Int)
applicationComponentSummary_count = Lens.lens (\ApplicationComponentSummary' {count} -> count) (\s@ApplicationComponentSummary' {} a -> s {count = a} :: ApplicationComponentSummary)

instance Data.FromJSON ApplicationComponentSummary where
  parseJSON =
    Data.withObject
      "ApplicationComponentSummary"
      ( \x ->
          ApplicationComponentSummary'
            Prelude.<$> (x Data..:? "appType")
            Prelude.<*> (x Data..:? "count")
      )

instance Prelude.Hashable ApplicationComponentSummary where
  hashWithSalt _salt ApplicationComponentSummary' {..} =
    _salt `Prelude.hashWithSalt` appType
      `Prelude.hashWithSalt` count

instance Prelude.NFData ApplicationComponentSummary where
  rnf ApplicationComponentSummary' {..} =
    Prelude.rnf appType `Prelude.seq` Prelude.rnf count
