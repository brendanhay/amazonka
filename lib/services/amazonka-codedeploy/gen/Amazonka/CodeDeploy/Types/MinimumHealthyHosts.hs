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
-- Module      : Amazonka.CodeDeploy.Types.MinimumHealthyHosts
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.MinimumHealthyHosts where

import Amazonka.CodeDeploy.Types.MinimumHealthyHostsType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about minimum healthy instance.
--
-- /See:/ 'newMinimumHealthyHosts' smart constructor.
data MinimumHealthyHosts = MinimumHealthyHosts'
  { -- | The minimum healthy instance type:
    --
    -- -   @HOST_COUNT@: The minimum number of healthy instances as an absolute
    --     value.
    --
    -- -   @FLEET_PERCENT@: The minimum number of healthy instances as a
    --     percentage of the total number of instances in the deployment.
    --
    -- In an example of nine instances, if a HOST_COUNT of six is specified,
    -- deploy to up to three instances at a time. The deployment is successful
    -- if six or more instances are deployed to successfully. Otherwise, the
    -- deployment fails. If a FLEET_PERCENT of 40 is specified, deploy to up to
    -- five instances at a time. The deployment is successful if four or more
    -- instances are deployed to successfully. Otherwise, the deployment fails.
    --
    -- In a call to the @GetDeploymentConfig@, CodeDeployDefault.OneAtATime
    -- returns a minimum healthy instance type of MOST_CONCURRENCY and a value
    -- of 1. This means a deployment to only one instance at a time. (You
    -- cannot set the type to MOST_CONCURRENCY, only to HOST_COUNT or
    -- FLEET_PERCENT.) In addition, with CodeDeployDefault.OneAtATime,
    -- CodeDeploy attempts to ensure that all instances but one are kept in a
    -- healthy state during the deployment. Although this allows one instance
    -- at a time to be taken offline for a new deployment, it also means that
    -- if the deployment to the last instance fails, the overall deployment is
    -- still successful.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/codedeploy/latest/userguide/instances-health.html CodeDeploy Instance Health>
    -- in the /CodeDeploy User Guide/.
    type' :: Prelude.Maybe MinimumHealthyHostsType,
    -- | The minimum healthy instance value.
    value :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MinimumHealthyHosts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'minimumHealthyHosts_type' - The minimum healthy instance type:
--
-- -   @HOST_COUNT@: The minimum number of healthy instances as an absolute
--     value.
--
-- -   @FLEET_PERCENT@: The minimum number of healthy instances as a
--     percentage of the total number of instances in the deployment.
--
-- In an example of nine instances, if a HOST_COUNT of six is specified,
-- deploy to up to three instances at a time. The deployment is successful
-- if six or more instances are deployed to successfully. Otherwise, the
-- deployment fails. If a FLEET_PERCENT of 40 is specified, deploy to up to
-- five instances at a time. The deployment is successful if four or more
-- instances are deployed to successfully. Otherwise, the deployment fails.
--
-- In a call to the @GetDeploymentConfig@, CodeDeployDefault.OneAtATime
-- returns a minimum healthy instance type of MOST_CONCURRENCY and a value
-- of 1. This means a deployment to only one instance at a time. (You
-- cannot set the type to MOST_CONCURRENCY, only to HOST_COUNT or
-- FLEET_PERCENT.) In addition, with CodeDeployDefault.OneAtATime,
-- CodeDeploy attempts to ensure that all instances but one are kept in a
-- healthy state during the deployment. Although this allows one instance
-- at a time to be taken offline for a new deployment, it also means that
-- if the deployment to the last instance fails, the overall deployment is
-- still successful.
--
-- For more information, see
-- <https://docs.aws.amazon.com/codedeploy/latest/userguide/instances-health.html CodeDeploy Instance Health>
-- in the /CodeDeploy User Guide/.
--
-- 'value', 'minimumHealthyHosts_value' - The minimum healthy instance value.
newMinimumHealthyHosts ::
  MinimumHealthyHosts
newMinimumHealthyHosts =
  MinimumHealthyHosts'
    { type' = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The minimum healthy instance type:
--
-- -   @HOST_COUNT@: The minimum number of healthy instances as an absolute
--     value.
--
-- -   @FLEET_PERCENT@: The minimum number of healthy instances as a
--     percentage of the total number of instances in the deployment.
--
-- In an example of nine instances, if a HOST_COUNT of six is specified,
-- deploy to up to three instances at a time. The deployment is successful
-- if six or more instances are deployed to successfully. Otherwise, the
-- deployment fails. If a FLEET_PERCENT of 40 is specified, deploy to up to
-- five instances at a time. The deployment is successful if four or more
-- instances are deployed to successfully. Otherwise, the deployment fails.
--
-- In a call to the @GetDeploymentConfig@, CodeDeployDefault.OneAtATime
-- returns a minimum healthy instance type of MOST_CONCURRENCY and a value
-- of 1. This means a deployment to only one instance at a time. (You
-- cannot set the type to MOST_CONCURRENCY, only to HOST_COUNT or
-- FLEET_PERCENT.) In addition, with CodeDeployDefault.OneAtATime,
-- CodeDeploy attempts to ensure that all instances but one are kept in a
-- healthy state during the deployment. Although this allows one instance
-- at a time to be taken offline for a new deployment, it also means that
-- if the deployment to the last instance fails, the overall deployment is
-- still successful.
--
-- For more information, see
-- <https://docs.aws.amazon.com/codedeploy/latest/userguide/instances-health.html CodeDeploy Instance Health>
-- in the /CodeDeploy User Guide/.
minimumHealthyHosts_type :: Lens.Lens' MinimumHealthyHosts (Prelude.Maybe MinimumHealthyHostsType)
minimumHealthyHosts_type = Lens.lens (\MinimumHealthyHosts' {type'} -> type') (\s@MinimumHealthyHosts' {} a -> s {type' = a} :: MinimumHealthyHosts)

-- | The minimum healthy instance value.
minimumHealthyHosts_value :: Lens.Lens' MinimumHealthyHosts (Prelude.Maybe Prelude.Int)
minimumHealthyHosts_value = Lens.lens (\MinimumHealthyHosts' {value} -> value) (\s@MinimumHealthyHosts' {} a -> s {value = a} :: MinimumHealthyHosts)

instance Data.FromJSON MinimumHealthyHosts where
  parseJSON =
    Data.withObject
      "MinimumHealthyHosts"
      ( \x ->
          MinimumHealthyHosts'
            Prelude.<$> (x Data..:? "type") Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable MinimumHealthyHosts where
  hashWithSalt _salt MinimumHealthyHosts' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` value

instance Prelude.NFData MinimumHealthyHosts where
  rnf MinimumHealthyHosts' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf value

instance Data.ToJSON MinimumHealthyHosts where
  toJSON MinimumHealthyHosts' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("type" Data..=) Prelude.<$> type',
            ("value" Data..=) Prelude.<$> value
          ]
      )
