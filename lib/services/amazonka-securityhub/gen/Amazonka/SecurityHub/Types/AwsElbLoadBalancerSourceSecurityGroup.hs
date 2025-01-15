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
-- Module      : Amazonka.SecurityHub.Types.AwsElbLoadBalancerSourceSecurityGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsElbLoadBalancerSourceSecurityGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the security group for the load balancer.
--
-- /See:/ 'newAwsElbLoadBalancerSourceSecurityGroup' smart constructor.
data AwsElbLoadBalancerSourceSecurityGroup = AwsElbLoadBalancerSourceSecurityGroup'
  { -- | The name of the security group.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The owner of the security group.
    ownerAlias :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsElbLoadBalancerSourceSecurityGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'awsElbLoadBalancerSourceSecurityGroup_groupName' - The name of the security group.
--
-- 'ownerAlias', 'awsElbLoadBalancerSourceSecurityGroup_ownerAlias' - The owner of the security group.
newAwsElbLoadBalancerSourceSecurityGroup ::
  AwsElbLoadBalancerSourceSecurityGroup
newAwsElbLoadBalancerSourceSecurityGroup =
  AwsElbLoadBalancerSourceSecurityGroup'
    { groupName =
        Prelude.Nothing,
      ownerAlias = Prelude.Nothing
    }

-- | The name of the security group.
awsElbLoadBalancerSourceSecurityGroup_groupName :: Lens.Lens' AwsElbLoadBalancerSourceSecurityGroup (Prelude.Maybe Prelude.Text)
awsElbLoadBalancerSourceSecurityGroup_groupName = Lens.lens (\AwsElbLoadBalancerSourceSecurityGroup' {groupName} -> groupName) (\s@AwsElbLoadBalancerSourceSecurityGroup' {} a -> s {groupName = a} :: AwsElbLoadBalancerSourceSecurityGroup)

-- | The owner of the security group.
awsElbLoadBalancerSourceSecurityGroup_ownerAlias :: Lens.Lens' AwsElbLoadBalancerSourceSecurityGroup (Prelude.Maybe Prelude.Text)
awsElbLoadBalancerSourceSecurityGroup_ownerAlias = Lens.lens (\AwsElbLoadBalancerSourceSecurityGroup' {ownerAlias} -> ownerAlias) (\s@AwsElbLoadBalancerSourceSecurityGroup' {} a -> s {ownerAlias = a} :: AwsElbLoadBalancerSourceSecurityGroup)

instance
  Data.FromJSON
    AwsElbLoadBalancerSourceSecurityGroup
  where
  parseJSON =
    Data.withObject
      "AwsElbLoadBalancerSourceSecurityGroup"
      ( \x ->
          AwsElbLoadBalancerSourceSecurityGroup'
            Prelude.<$> (x Data..:? "GroupName")
            Prelude.<*> (x Data..:? "OwnerAlias")
      )

instance
  Prelude.Hashable
    AwsElbLoadBalancerSourceSecurityGroup
  where
  hashWithSalt
    _salt
    AwsElbLoadBalancerSourceSecurityGroup' {..} =
      _salt
        `Prelude.hashWithSalt` groupName
        `Prelude.hashWithSalt` ownerAlias

instance
  Prelude.NFData
    AwsElbLoadBalancerSourceSecurityGroup
  where
  rnf AwsElbLoadBalancerSourceSecurityGroup' {..} =
    Prelude.rnf groupName `Prelude.seq`
      Prelude.rnf ownerAlias

instance
  Data.ToJSON
    AwsElbLoadBalancerSourceSecurityGroup
  where
  toJSON AwsElbLoadBalancerSourceSecurityGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GroupName" Data..=) Prelude.<$> groupName,
            ("OwnerAlias" Data..=) Prelude.<$> ownerAlias
          ]
      )
