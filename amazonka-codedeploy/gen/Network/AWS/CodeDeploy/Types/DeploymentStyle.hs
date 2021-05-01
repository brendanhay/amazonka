{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentStyle
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.DeploymentStyle where

import Network.AWS.CodeDeploy.Types.DeploymentOption
import Network.AWS.CodeDeploy.Types.DeploymentType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the type of deployment, either in-place or
-- blue\/green, you want to run and whether to route deployment traffic
-- behind a load balancer.
--
-- /See:/ 'newDeploymentStyle' smart constructor.
data DeploymentStyle = DeploymentStyle'
  { -- | Indicates whether to run an in-place deployment or a blue\/green
    -- deployment.
    deploymentType :: Prelude.Maybe DeploymentType,
    -- | Indicates whether to route deployment traffic behind a load balancer.
    deploymentOption :: Prelude.Maybe DeploymentOption
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeploymentStyle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentType', 'deploymentStyle_deploymentType' - Indicates whether to run an in-place deployment or a blue\/green
-- deployment.
--
-- 'deploymentOption', 'deploymentStyle_deploymentOption' - Indicates whether to route deployment traffic behind a load balancer.
newDeploymentStyle ::
  DeploymentStyle
newDeploymentStyle =
  DeploymentStyle'
    { deploymentType = Prelude.Nothing,
      deploymentOption = Prelude.Nothing
    }

-- | Indicates whether to run an in-place deployment or a blue\/green
-- deployment.
deploymentStyle_deploymentType :: Lens.Lens' DeploymentStyle (Prelude.Maybe DeploymentType)
deploymentStyle_deploymentType = Lens.lens (\DeploymentStyle' {deploymentType} -> deploymentType) (\s@DeploymentStyle' {} a -> s {deploymentType = a} :: DeploymentStyle)

-- | Indicates whether to route deployment traffic behind a load balancer.
deploymentStyle_deploymentOption :: Lens.Lens' DeploymentStyle (Prelude.Maybe DeploymentOption)
deploymentStyle_deploymentOption = Lens.lens (\DeploymentStyle' {deploymentOption} -> deploymentOption) (\s@DeploymentStyle' {} a -> s {deploymentOption = a} :: DeploymentStyle)

instance Prelude.FromJSON DeploymentStyle where
  parseJSON =
    Prelude.withObject
      "DeploymentStyle"
      ( \x ->
          DeploymentStyle'
            Prelude.<$> (x Prelude..:? "deploymentType")
            Prelude.<*> (x Prelude..:? "deploymentOption")
      )

instance Prelude.Hashable DeploymentStyle

instance Prelude.NFData DeploymentStyle

instance Prelude.ToJSON DeploymentStyle where
  toJSON DeploymentStyle' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("deploymentType" Prelude..=)
              Prelude.<$> deploymentType,
            ("deploymentOption" Prelude..=)
              Prelude.<$> deploymentOption
          ]
      )
