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
-- Module      : Network.AWS.CodeDeploy.Types.RelatedDeployments
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.RelatedDeployments where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about deployments related to the specified deployment.
--
-- /See:/ 'newRelatedDeployments' smart constructor.
data RelatedDeployments = RelatedDeployments'
  { -- | The deployment ID of the root deployment that triggered this deployment.
    autoUpdateOutdatedInstancesRootDeploymentId :: Prelude.Maybe Prelude.Text,
    -- | The deployment IDs of \'auto-update outdated instances\' deployments
    -- triggered by this deployment.
    autoUpdateOutdatedInstancesDeploymentIds :: Prelude.Maybe [Prelude.Text]
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
-- 'autoUpdateOutdatedInstancesRootDeploymentId', 'relatedDeployments_autoUpdateOutdatedInstancesRootDeploymentId' - The deployment ID of the root deployment that triggered this deployment.
--
-- 'autoUpdateOutdatedInstancesDeploymentIds', 'relatedDeployments_autoUpdateOutdatedInstancesDeploymentIds' - The deployment IDs of \'auto-update outdated instances\' deployments
-- triggered by this deployment.
newRelatedDeployments ::
  RelatedDeployments
newRelatedDeployments =
  RelatedDeployments'
    { autoUpdateOutdatedInstancesRootDeploymentId =
        Prelude.Nothing,
      autoUpdateOutdatedInstancesDeploymentIds =
        Prelude.Nothing
    }

-- | The deployment ID of the root deployment that triggered this deployment.
relatedDeployments_autoUpdateOutdatedInstancesRootDeploymentId :: Lens.Lens' RelatedDeployments (Prelude.Maybe Prelude.Text)
relatedDeployments_autoUpdateOutdatedInstancesRootDeploymentId = Lens.lens (\RelatedDeployments' {autoUpdateOutdatedInstancesRootDeploymentId} -> autoUpdateOutdatedInstancesRootDeploymentId) (\s@RelatedDeployments' {} a -> s {autoUpdateOutdatedInstancesRootDeploymentId = a} :: RelatedDeployments)

-- | The deployment IDs of \'auto-update outdated instances\' deployments
-- triggered by this deployment.
relatedDeployments_autoUpdateOutdatedInstancesDeploymentIds :: Lens.Lens' RelatedDeployments (Prelude.Maybe [Prelude.Text])
relatedDeployments_autoUpdateOutdatedInstancesDeploymentIds = Lens.lens (\RelatedDeployments' {autoUpdateOutdatedInstancesDeploymentIds} -> autoUpdateOutdatedInstancesDeploymentIds) (\s@RelatedDeployments' {} a -> s {autoUpdateOutdatedInstancesDeploymentIds = a} :: RelatedDeployments) Prelude.. Lens.mapping Lens._Coerce

instance Core.FromJSON RelatedDeployments where
  parseJSON =
    Core.withObject
      "RelatedDeployments"
      ( \x ->
          RelatedDeployments'
            Prelude.<$> ( x
                            Core..:? "autoUpdateOutdatedInstancesRootDeploymentId"
                        )
            Prelude.<*> ( x
                            Core..:? "autoUpdateOutdatedInstancesDeploymentIds"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable RelatedDeployments

instance Prelude.NFData RelatedDeployments
