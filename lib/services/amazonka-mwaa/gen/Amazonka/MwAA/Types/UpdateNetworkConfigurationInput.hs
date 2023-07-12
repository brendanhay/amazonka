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
-- Module      : Amazonka.MwAA.Types.UpdateNetworkConfigurationInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MwAA.Types.UpdateNetworkConfigurationInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines the VPC networking components used to secure and enable network
-- traffic between the Amazon Web Services resources for your environment.
-- To learn more, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/networking-about.html About networking on Amazon MWAA>.
--
-- /See:/ 'newUpdateNetworkConfigurationInput' smart constructor.
data UpdateNetworkConfigurationInput = UpdateNetworkConfigurationInput'
  { -- | A list of security group IDs. A security group must be attached to the
    -- same VPC as the subnets. To learn more, see
    -- <https://docs.aws.amazon.com/mwaa/latest/userguide/vpc-security.html Security in your VPC on Amazon MWAA>.
    securityGroupIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateNetworkConfigurationInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'updateNetworkConfigurationInput_securityGroupIds' - A list of security group IDs. A security group must be attached to the
-- same VPC as the subnets. To learn more, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/vpc-security.html Security in your VPC on Amazon MWAA>.
newUpdateNetworkConfigurationInput ::
  -- | 'securityGroupIds'
  Prelude.NonEmpty Prelude.Text ->
  UpdateNetworkConfigurationInput
newUpdateNetworkConfigurationInput pSecurityGroupIds_ =
  UpdateNetworkConfigurationInput'
    { securityGroupIds =
        Lens.coerced Lens.# pSecurityGroupIds_
    }

-- | A list of security group IDs. A security group must be attached to the
-- same VPC as the subnets. To learn more, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/vpc-security.html Security in your VPC on Amazon MWAA>.
updateNetworkConfigurationInput_securityGroupIds :: Lens.Lens' UpdateNetworkConfigurationInput (Prelude.NonEmpty Prelude.Text)
updateNetworkConfigurationInput_securityGroupIds = Lens.lens (\UpdateNetworkConfigurationInput' {securityGroupIds} -> securityGroupIds) (\s@UpdateNetworkConfigurationInput' {} a -> s {securityGroupIds = a} :: UpdateNetworkConfigurationInput) Prelude.. Lens.coerced

instance
  Prelude.Hashable
    UpdateNetworkConfigurationInput
  where
  hashWithSalt
    _salt
    UpdateNetworkConfigurationInput' {..} =
      _salt `Prelude.hashWithSalt` securityGroupIds

instance
  Prelude.NFData
    UpdateNetworkConfigurationInput
  where
  rnf UpdateNetworkConfigurationInput' {..} =
    Prelude.rnf securityGroupIds

instance Data.ToJSON UpdateNetworkConfigurationInput where
  toJSON UpdateNetworkConfigurationInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("SecurityGroupIds" Data..= securityGroupIds)
          ]
      )
