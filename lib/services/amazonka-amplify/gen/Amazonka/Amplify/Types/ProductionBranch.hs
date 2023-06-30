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
-- Module      : Amazonka.Amplify.Types.ProductionBranch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Amplify.Types.ProductionBranch where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the information about a production branch for an Amplify app.
--
-- /See:/ 'newProductionBranch' smart constructor.
data ProductionBranch = ProductionBranch'
  { -- | The branch name for the production branch.
    branchName :: Prelude.Maybe Prelude.Text,
    -- | The last deploy time of the production branch.
    lastDeployTime :: Prelude.Maybe Data.POSIX,
    -- | The status of the production branch.
    status :: Prelude.Maybe Prelude.Text,
    -- | The thumbnail URL for the production branch.
    thumbnailUrl :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProductionBranch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'branchName', 'productionBranch_branchName' - The branch name for the production branch.
--
-- 'lastDeployTime', 'productionBranch_lastDeployTime' - The last deploy time of the production branch.
--
-- 'status', 'productionBranch_status' - The status of the production branch.
--
-- 'thumbnailUrl', 'productionBranch_thumbnailUrl' - The thumbnail URL for the production branch.
newProductionBranch ::
  ProductionBranch
newProductionBranch =
  ProductionBranch'
    { branchName = Prelude.Nothing,
      lastDeployTime = Prelude.Nothing,
      status = Prelude.Nothing,
      thumbnailUrl = Prelude.Nothing
    }

-- | The branch name for the production branch.
productionBranch_branchName :: Lens.Lens' ProductionBranch (Prelude.Maybe Prelude.Text)
productionBranch_branchName = Lens.lens (\ProductionBranch' {branchName} -> branchName) (\s@ProductionBranch' {} a -> s {branchName = a} :: ProductionBranch)

-- | The last deploy time of the production branch.
productionBranch_lastDeployTime :: Lens.Lens' ProductionBranch (Prelude.Maybe Prelude.UTCTime)
productionBranch_lastDeployTime = Lens.lens (\ProductionBranch' {lastDeployTime} -> lastDeployTime) (\s@ProductionBranch' {} a -> s {lastDeployTime = a} :: ProductionBranch) Prelude.. Lens.mapping Data._Time

-- | The status of the production branch.
productionBranch_status :: Lens.Lens' ProductionBranch (Prelude.Maybe Prelude.Text)
productionBranch_status = Lens.lens (\ProductionBranch' {status} -> status) (\s@ProductionBranch' {} a -> s {status = a} :: ProductionBranch)

-- | The thumbnail URL for the production branch.
productionBranch_thumbnailUrl :: Lens.Lens' ProductionBranch (Prelude.Maybe Prelude.Text)
productionBranch_thumbnailUrl = Lens.lens (\ProductionBranch' {thumbnailUrl} -> thumbnailUrl) (\s@ProductionBranch' {} a -> s {thumbnailUrl = a} :: ProductionBranch)

instance Data.FromJSON ProductionBranch where
  parseJSON =
    Data.withObject
      "ProductionBranch"
      ( \x ->
          ProductionBranch'
            Prelude.<$> (x Data..:? "branchName")
            Prelude.<*> (x Data..:? "lastDeployTime")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "thumbnailUrl")
      )

instance Prelude.Hashable ProductionBranch where
  hashWithSalt _salt ProductionBranch' {..} =
    _salt
      `Prelude.hashWithSalt` branchName
      `Prelude.hashWithSalt` lastDeployTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` thumbnailUrl

instance Prelude.NFData ProductionBranch where
  rnf ProductionBranch' {..} =
    Prelude.rnf branchName
      `Prelude.seq` Prelude.rnf lastDeployTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf thumbnailUrl
