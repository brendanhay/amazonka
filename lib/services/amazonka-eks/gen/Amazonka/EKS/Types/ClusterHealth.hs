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
-- Module      : Amazonka.EKS.Types.ClusterHealth
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.ClusterHealth where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EKS.Types.ClusterIssue
import qualified Amazonka.Prelude as Prelude

-- | An object representing the health of your local Amazon EKS cluster on an
-- Amazon Web Services Outpost. You can\'t use this API with an Amazon EKS
-- cluster on the Amazon Web Services cloud.
--
-- /See:/ 'newClusterHealth' smart constructor.
data ClusterHealth = ClusterHealth'
  { -- | An object representing the health issues of your local Amazon EKS
    -- cluster on an Amazon Web Services Outpost.
    issues :: Prelude.Maybe [ClusterIssue]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClusterHealth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'issues', 'clusterHealth_issues' - An object representing the health issues of your local Amazon EKS
-- cluster on an Amazon Web Services Outpost.
newClusterHealth ::
  ClusterHealth
newClusterHealth =
  ClusterHealth' {issues = Prelude.Nothing}

-- | An object representing the health issues of your local Amazon EKS
-- cluster on an Amazon Web Services Outpost.
clusterHealth_issues :: Lens.Lens' ClusterHealth (Prelude.Maybe [ClusterIssue])
clusterHealth_issues = Lens.lens (\ClusterHealth' {issues} -> issues) (\s@ClusterHealth' {} a -> s {issues = a} :: ClusterHealth) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON ClusterHealth where
  parseJSON =
    Core.withObject
      "ClusterHealth"
      ( \x ->
          ClusterHealth'
            Prelude.<$> (x Core..:? "issues" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ClusterHealth where
  hashWithSalt _salt ClusterHealth' {..} =
    _salt `Prelude.hashWithSalt` issues

instance Prelude.NFData ClusterHealth where
  rnf ClusterHealth' {..} = Prelude.rnf issues
