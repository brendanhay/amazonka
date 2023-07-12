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
-- Module      : Amazonka.SecurityHub.Types.AwsElbLoadBalancerInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsElbLoadBalancerInstance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about an EC2 instance for a load balancer.
--
-- /See:/ 'newAwsElbLoadBalancerInstance' smart constructor.
data AwsElbLoadBalancerInstance = AwsElbLoadBalancerInstance'
  { -- | The instance identifier.
    instanceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsElbLoadBalancerInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'awsElbLoadBalancerInstance_instanceId' - The instance identifier.
newAwsElbLoadBalancerInstance ::
  AwsElbLoadBalancerInstance
newAwsElbLoadBalancerInstance =
  AwsElbLoadBalancerInstance'
    { instanceId =
        Prelude.Nothing
    }

-- | The instance identifier.
awsElbLoadBalancerInstance_instanceId :: Lens.Lens' AwsElbLoadBalancerInstance (Prelude.Maybe Prelude.Text)
awsElbLoadBalancerInstance_instanceId = Lens.lens (\AwsElbLoadBalancerInstance' {instanceId} -> instanceId) (\s@AwsElbLoadBalancerInstance' {} a -> s {instanceId = a} :: AwsElbLoadBalancerInstance)

instance Data.FromJSON AwsElbLoadBalancerInstance where
  parseJSON =
    Data.withObject
      "AwsElbLoadBalancerInstance"
      ( \x ->
          AwsElbLoadBalancerInstance'
            Prelude.<$> (x Data..:? "InstanceId")
      )

instance Prelude.Hashable AwsElbLoadBalancerInstance where
  hashWithSalt _salt AwsElbLoadBalancerInstance' {..} =
    _salt `Prelude.hashWithSalt` instanceId

instance Prelude.NFData AwsElbLoadBalancerInstance where
  rnf AwsElbLoadBalancerInstance' {..} =
    Prelude.rnf instanceId

instance Data.ToJSON AwsElbLoadBalancerInstance where
  toJSON AwsElbLoadBalancerInstance' {..} =
    Data.object
      ( Prelude.catMaybes
          [("InstanceId" Data..=) Prelude.<$> instanceId]
      )
