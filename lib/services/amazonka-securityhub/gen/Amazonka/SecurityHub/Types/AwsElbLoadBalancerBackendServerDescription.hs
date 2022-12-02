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
-- Module      : Amazonka.SecurityHub.Types.AwsElbLoadBalancerBackendServerDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsElbLoadBalancerBackendServerDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the configuration of an EC2 instance for the
-- load balancer.
--
-- /See:/ 'newAwsElbLoadBalancerBackendServerDescription' smart constructor.
data AwsElbLoadBalancerBackendServerDescription = AwsElbLoadBalancerBackendServerDescription'
  { -- | The names of the policies that are enabled for the EC2 instance.
    policyNames :: Prelude.Maybe [Prelude.Text],
    -- | The port on which the EC2 instance is listening.
    instancePort :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsElbLoadBalancerBackendServerDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyNames', 'awsElbLoadBalancerBackendServerDescription_policyNames' - The names of the policies that are enabled for the EC2 instance.
--
-- 'instancePort', 'awsElbLoadBalancerBackendServerDescription_instancePort' - The port on which the EC2 instance is listening.
newAwsElbLoadBalancerBackendServerDescription ::
  AwsElbLoadBalancerBackendServerDescription
newAwsElbLoadBalancerBackendServerDescription =
  AwsElbLoadBalancerBackendServerDescription'
    { policyNames =
        Prelude.Nothing,
      instancePort = Prelude.Nothing
    }

-- | The names of the policies that are enabled for the EC2 instance.
awsElbLoadBalancerBackendServerDescription_policyNames :: Lens.Lens' AwsElbLoadBalancerBackendServerDescription (Prelude.Maybe [Prelude.Text])
awsElbLoadBalancerBackendServerDescription_policyNames = Lens.lens (\AwsElbLoadBalancerBackendServerDescription' {policyNames} -> policyNames) (\s@AwsElbLoadBalancerBackendServerDescription' {} a -> s {policyNames = a} :: AwsElbLoadBalancerBackendServerDescription) Prelude.. Lens.mapping Lens.coerced

-- | The port on which the EC2 instance is listening.
awsElbLoadBalancerBackendServerDescription_instancePort :: Lens.Lens' AwsElbLoadBalancerBackendServerDescription (Prelude.Maybe Prelude.Int)
awsElbLoadBalancerBackendServerDescription_instancePort = Lens.lens (\AwsElbLoadBalancerBackendServerDescription' {instancePort} -> instancePort) (\s@AwsElbLoadBalancerBackendServerDescription' {} a -> s {instancePort = a} :: AwsElbLoadBalancerBackendServerDescription)

instance
  Data.FromJSON
    AwsElbLoadBalancerBackendServerDescription
  where
  parseJSON =
    Data.withObject
      "AwsElbLoadBalancerBackendServerDescription"
      ( \x ->
          AwsElbLoadBalancerBackendServerDescription'
            Prelude.<$> (x Data..:? "PolicyNames" Data..!= Prelude.mempty)
              Prelude.<*> (x Data..:? "InstancePort")
      )

instance
  Prelude.Hashable
    AwsElbLoadBalancerBackendServerDescription
  where
  hashWithSalt
    _salt
    AwsElbLoadBalancerBackendServerDescription' {..} =
      _salt `Prelude.hashWithSalt` policyNames
        `Prelude.hashWithSalt` instancePort

instance
  Prelude.NFData
    AwsElbLoadBalancerBackendServerDescription
  where
  rnf AwsElbLoadBalancerBackendServerDescription' {..} =
    Prelude.rnf policyNames
      `Prelude.seq` Prelude.rnf instancePort

instance
  Data.ToJSON
    AwsElbLoadBalancerBackendServerDescription
  where
  toJSON
    AwsElbLoadBalancerBackendServerDescription' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("PolicyNames" Data..=) Prelude.<$> policyNames,
              ("InstancePort" Data..=) Prelude.<$> instancePort
            ]
        )
