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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsElbLoadBalancerSourceSecurityGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the security group for the load balancer.
--
-- /See:/ 'newAwsElbLoadBalancerSourceSecurityGroup' smart constructor.
data AwsElbLoadBalancerSourceSecurityGroup = AwsElbLoadBalancerSourceSecurityGroup'
  { -- | The owner of the security group.
    ownerAlias :: Prelude.Maybe Prelude.Text,
    -- | The name of the security group.
    groupName :: Prelude.Maybe Prelude.Text
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
-- 'ownerAlias', 'awsElbLoadBalancerSourceSecurityGroup_ownerAlias' - The owner of the security group.
--
-- 'groupName', 'awsElbLoadBalancerSourceSecurityGroup_groupName' - The name of the security group.
newAwsElbLoadBalancerSourceSecurityGroup ::
  AwsElbLoadBalancerSourceSecurityGroup
newAwsElbLoadBalancerSourceSecurityGroup =
  AwsElbLoadBalancerSourceSecurityGroup'
    { ownerAlias =
        Prelude.Nothing,
      groupName = Prelude.Nothing
    }

-- | The owner of the security group.
awsElbLoadBalancerSourceSecurityGroup_ownerAlias :: Lens.Lens' AwsElbLoadBalancerSourceSecurityGroup (Prelude.Maybe Prelude.Text)
awsElbLoadBalancerSourceSecurityGroup_ownerAlias = Lens.lens (\AwsElbLoadBalancerSourceSecurityGroup' {ownerAlias} -> ownerAlias) (\s@AwsElbLoadBalancerSourceSecurityGroup' {} a -> s {ownerAlias = a} :: AwsElbLoadBalancerSourceSecurityGroup)

-- | The name of the security group.
awsElbLoadBalancerSourceSecurityGroup_groupName :: Lens.Lens' AwsElbLoadBalancerSourceSecurityGroup (Prelude.Maybe Prelude.Text)
awsElbLoadBalancerSourceSecurityGroup_groupName = Lens.lens (\AwsElbLoadBalancerSourceSecurityGroup' {groupName} -> groupName) (\s@AwsElbLoadBalancerSourceSecurityGroup' {} a -> s {groupName = a} :: AwsElbLoadBalancerSourceSecurityGroup)

instance
  Core.FromJSON
    AwsElbLoadBalancerSourceSecurityGroup
  where
  parseJSON =
    Core.withObject
      "AwsElbLoadBalancerSourceSecurityGroup"
      ( \x ->
          AwsElbLoadBalancerSourceSecurityGroup'
            Prelude.<$> (x Core..:? "OwnerAlias")
            Prelude.<*> (x Core..:? "GroupName")
      )

instance
  Prelude.Hashable
    AwsElbLoadBalancerSourceSecurityGroup
  where
  hashWithSalt
    _salt
    AwsElbLoadBalancerSourceSecurityGroup' {..} =
      _salt `Prelude.hashWithSalt` ownerAlias
        `Prelude.hashWithSalt` groupName

instance
  Prelude.NFData
    AwsElbLoadBalancerSourceSecurityGroup
  where
  rnf AwsElbLoadBalancerSourceSecurityGroup' {..} =
    Prelude.rnf ownerAlias
      `Prelude.seq` Prelude.rnf groupName

instance
  Core.ToJSON
    AwsElbLoadBalancerSourceSecurityGroup
  where
  toJSON AwsElbLoadBalancerSourceSecurityGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("OwnerAlias" Core..=) Prelude.<$> ownerAlias,
            ("GroupName" Core..=) Prelude.<$> groupName
          ]
      )
