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
-- Module      : Amazonka.CodeDeploy.Types.RelatedDeployments
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.RelatedDeployments where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about deployments related to the specified deployment.
--
-- /See:/ 'newRelatedDeployments' smart constructor.
data RelatedDeployments = RelatedDeployments'
  { -- | The deployment IDs of \'auto-update outdated instances\' deployments
    -- triggered by this deployment.
    autoUpdateOutdatedInstancesDeploymentIds :: Prelude.Maybe [Prelude.Text],
    -- | The deployment ID of the root deployment that triggered this deployment.
    autoUpdateOutdatedInstancesRootDeploymentId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RelatedDeployments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoUpdateOutdatedInstancesDeploymentIds', 'relatedDeployments_autoUpdateOutdatedInstancesDeploymentIds' - The deployment IDs of \'auto-update outdated instances\' deployments
-- triggered by this deployment.
--
-- 'autoUpdateOutdatedInstancesRootDeploymentId', 'relatedDeployments_autoUpdateOutdatedInstancesRootDeploymentId' - The deployment ID of the root deployment that triggered this deployment.
newRelatedDeployments ::
  RelatedDeployments
newRelatedDeployments =
  RelatedDeployments'
    { autoUpdateOutdatedInstancesDeploymentIds =
        Prelude.Nothing,
      autoUpdateOutdatedInstancesRootDeploymentId =
        Prelude.Nothing
    }

-- | The deployment IDs of \'auto-update outdated instances\' deployments
-- triggered by this deployment.
relatedDeployments_autoUpdateOutdatedInstancesDeploymentIds :: Lens.Lens' RelatedDeployments (Prelude.Maybe [Prelude.Text])
relatedDeployments_autoUpdateOutdatedInstancesDeploymentIds = Lens.lens (\RelatedDeployments' {autoUpdateOutdatedInstancesDeploymentIds} -> autoUpdateOutdatedInstancesDeploymentIds) (\s@RelatedDeployments' {} a -> s {autoUpdateOutdatedInstancesDeploymentIds = a} :: RelatedDeployments) Prelude.. Lens.mapping Lens.coerced

-- | The deployment ID of the root deployment that triggered this deployment.
relatedDeployments_autoUpdateOutdatedInstancesRootDeploymentId :: Lens.Lens' RelatedDeployments (Prelude.Maybe Prelude.Text)
relatedDeployments_autoUpdateOutdatedInstancesRootDeploymentId = Lens.lens (\RelatedDeployments' {autoUpdateOutdatedInstancesRootDeploymentId} -> autoUpdateOutdatedInstancesRootDeploymentId) (\s@RelatedDeployments' {} a -> s {autoUpdateOutdatedInstancesRootDeploymentId = a} :: RelatedDeployments)

instance Data.FromJSON RelatedDeployments where
  parseJSON =
    Data.withObject
      "RelatedDeployments"
      ( \x ->
          RelatedDeployments'
            Prelude.<$> ( x
                            Data..:? "autoUpdateOutdatedInstancesDeploymentIds"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "autoUpdateOutdatedInstancesRootDeploymentId"
                        )
      )

instance Prelude.Hashable RelatedDeployments where
  hashWithSalt _salt RelatedDeployments' {..} =
    _salt
      `Prelude.hashWithSalt` autoUpdateOutdatedInstancesDeploymentIds
      `Prelude.hashWithSalt` autoUpdateOutdatedInstancesRootDeploymentId

instance Prelude.NFData RelatedDeployments where
  rnf RelatedDeployments' {..} =
    Prelude.rnf
      autoUpdateOutdatedInstancesDeploymentIds
      `Prelude.seq` Prelude.rnf
        autoUpdateOutdatedInstancesRootDeploymentId
