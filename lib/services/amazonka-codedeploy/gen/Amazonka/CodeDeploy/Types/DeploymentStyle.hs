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
-- Module      : Amazonka.CodeDeploy.Types.DeploymentStyle
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.DeploymentStyle where

import Amazonka.CodeDeploy.Types.DeploymentOption
import Amazonka.CodeDeploy.Types.DeploymentType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the type of deployment, either in-place or
-- blue\/green, you want to run and whether to route deployment traffic
-- behind a load balancer.
--
-- /See:/ 'newDeploymentStyle' smart constructor.
data DeploymentStyle = DeploymentStyle'
  { -- | Indicates whether to route deployment traffic behind a load balancer.
    deploymentOption :: Prelude.Maybe DeploymentOption,
    -- | Indicates whether to run an in-place deployment or a blue\/green
    -- deployment.
    deploymentType :: Prelude.Maybe DeploymentType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeploymentStyle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentOption', 'deploymentStyle_deploymentOption' - Indicates whether to route deployment traffic behind a load balancer.
--
-- 'deploymentType', 'deploymentStyle_deploymentType' - Indicates whether to run an in-place deployment or a blue\/green
-- deployment.
newDeploymentStyle ::
  DeploymentStyle
newDeploymentStyle =
  DeploymentStyle'
    { deploymentOption =
        Prelude.Nothing,
      deploymentType = Prelude.Nothing
    }

-- | Indicates whether to route deployment traffic behind a load balancer.
deploymentStyle_deploymentOption :: Lens.Lens' DeploymentStyle (Prelude.Maybe DeploymentOption)
deploymentStyle_deploymentOption = Lens.lens (\DeploymentStyle' {deploymentOption} -> deploymentOption) (\s@DeploymentStyle' {} a -> s {deploymentOption = a} :: DeploymentStyle)

-- | Indicates whether to run an in-place deployment or a blue\/green
-- deployment.
deploymentStyle_deploymentType :: Lens.Lens' DeploymentStyle (Prelude.Maybe DeploymentType)
deploymentStyle_deploymentType = Lens.lens (\DeploymentStyle' {deploymentType} -> deploymentType) (\s@DeploymentStyle' {} a -> s {deploymentType = a} :: DeploymentStyle)

instance Data.FromJSON DeploymentStyle where
  parseJSON =
    Data.withObject
      "DeploymentStyle"
      ( \x ->
          DeploymentStyle'
            Prelude.<$> (x Data..:? "deploymentOption")
            Prelude.<*> (x Data..:? "deploymentType")
      )

instance Prelude.Hashable DeploymentStyle where
  hashWithSalt _salt DeploymentStyle' {..} =
    _salt
      `Prelude.hashWithSalt` deploymentOption
      `Prelude.hashWithSalt` deploymentType

instance Prelude.NFData DeploymentStyle where
  rnf DeploymentStyle' {..} =
    Prelude.rnf deploymentOption
      `Prelude.seq` Prelude.rnf deploymentType

instance Data.ToJSON DeploymentStyle where
  toJSON DeploymentStyle' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("deploymentOption" Data..=)
              Prelude.<$> deploymentOption,
            ("deploymentType" Data..=)
              Prelude.<$> deploymentType
          ]
      )
