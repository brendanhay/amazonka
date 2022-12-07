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
-- Module      : Amazonka.GroundStation.Types.SecurityDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.SecurityDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about endpoints.
--
-- /See:/ 'newSecurityDetails' smart constructor.
data SecurityDetails = SecurityDetails'
  { -- | ARN to a role needed for connecting streams to your instances.
    roleArn :: Prelude.Text,
    -- | The security groups to attach to the elastic network interfaces.
    securityGroupIds :: [Prelude.Text],
    -- | A list of subnets where AWS Ground Station places elastic network
    -- interfaces to send streams to your instances.
    subnetIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SecurityDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'securityDetails_roleArn' - ARN to a role needed for connecting streams to your instances.
--
-- 'securityGroupIds', 'securityDetails_securityGroupIds' - The security groups to attach to the elastic network interfaces.
--
-- 'subnetIds', 'securityDetails_subnetIds' - A list of subnets where AWS Ground Station places elastic network
-- interfaces to send streams to your instances.
newSecurityDetails ::
  -- | 'roleArn'
  Prelude.Text ->
  SecurityDetails
newSecurityDetails pRoleArn_ =
  SecurityDetails'
    { roleArn = pRoleArn_,
      securityGroupIds = Prelude.mempty,
      subnetIds = Prelude.mempty
    }

-- | ARN to a role needed for connecting streams to your instances.
securityDetails_roleArn :: Lens.Lens' SecurityDetails Prelude.Text
securityDetails_roleArn = Lens.lens (\SecurityDetails' {roleArn} -> roleArn) (\s@SecurityDetails' {} a -> s {roleArn = a} :: SecurityDetails)

-- | The security groups to attach to the elastic network interfaces.
securityDetails_securityGroupIds :: Lens.Lens' SecurityDetails [Prelude.Text]
securityDetails_securityGroupIds = Lens.lens (\SecurityDetails' {securityGroupIds} -> securityGroupIds) (\s@SecurityDetails' {} a -> s {securityGroupIds = a} :: SecurityDetails) Prelude.. Lens.coerced

-- | A list of subnets where AWS Ground Station places elastic network
-- interfaces to send streams to your instances.
securityDetails_subnetIds :: Lens.Lens' SecurityDetails [Prelude.Text]
securityDetails_subnetIds = Lens.lens (\SecurityDetails' {subnetIds} -> subnetIds) (\s@SecurityDetails' {} a -> s {subnetIds = a} :: SecurityDetails) Prelude.. Lens.coerced

instance Data.FromJSON SecurityDetails where
  parseJSON =
    Data.withObject
      "SecurityDetails"
      ( \x ->
          SecurityDetails'
            Prelude.<$> (x Data..: "roleArn")
            Prelude.<*> ( x Data..:? "securityGroupIds"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "subnetIds" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable SecurityDetails where
  hashWithSalt _salt SecurityDetails' {..} =
    _salt `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` subnetIds

instance Prelude.NFData SecurityDetails where
  rnf SecurityDetails' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf subnetIds

instance Data.ToJSON SecurityDetails where
  toJSON SecurityDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("roleArn" Data..= roleArn),
            Prelude.Just
              ("securityGroupIds" Data..= securityGroupIds),
            Prelude.Just ("subnetIds" Data..= subnetIds)
          ]
      )
