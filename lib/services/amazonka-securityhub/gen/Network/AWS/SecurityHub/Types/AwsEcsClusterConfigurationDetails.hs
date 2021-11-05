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
-- Module      : Network.AWS.SecurityHub.Types.AwsEcsClusterConfigurationDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsEcsClusterConfigurationDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SecurityHub.Types.AwsEcsClusterConfigurationExecuteCommandConfigurationDetails

-- | The run command configuration for the cluster.
--
-- /See:/ 'newAwsEcsClusterConfigurationDetails' smart constructor.
data AwsEcsClusterConfigurationDetails = AwsEcsClusterConfigurationDetails'
  { -- | Contains the run command configuration for the cluster.
    executeCommandConfiguration :: Prelude.Maybe AwsEcsClusterConfigurationExecuteCommandConfigurationDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsClusterConfigurationDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'executeCommandConfiguration', 'awsEcsClusterConfigurationDetails_executeCommandConfiguration' - Contains the run command configuration for the cluster.
newAwsEcsClusterConfigurationDetails ::
  AwsEcsClusterConfigurationDetails
newAwsEcsClusterConfigurationDetails =
  AwsEcsClusterConfigurationDetails'
    { executeCommandConfiguration =
        Prelude.Nothing
    }

-- | Contains the run command configuration for the cluster.
awsEcsClusterConfigurationDetails_executeCommandConfiguration :: Lens.Lens' AwsEcsClusterConfigurationDetails (Prelude.Maybe AwsEcsClusterConfigurationExecuteCommandConfigurationDetails)
awsEcsClusterConfigurationDetails_executeCommandConfiguration = Lens.lens (\AwsEcsClusterConfigurationDetails' {executeCommandConfiguration} -> executeCommandConfiguration) (\s@AwsEcsClusterConfigurationDetails' {} a -> s {executeCommandConfiguration = a} :: AwsEcsClusterConfigurationDetails)

instance
  Core.FromJSON
    AwsEcsClusterConfigurationDetails
  where
  parseJSON =
    Core.withObject
      "AwsEcsClusterConfigurationDetails"
      ( \x ->
          AwsEcsClusterConfigurationDetails'
            Prelude.<$> (x Core..:? "ExecuteCommandConfiguration")
      )

instance
  Prelude.Hashable
    AwsEcsClusterConfigurationDetails

instance
  Prelude.NFData
    AwsEcsClusterConfigurationDetails

instance
  Core.ToJSON
    AwsEcsClusterConfigurationDetails
  where
  toJSON AwsEcsClusterConfigurationDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ExecuteCommandConfiguration" Core..=)
              Prelude.<$> executeCommandConfiguration
          ]
      )
