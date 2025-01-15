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
-- Module      : Amazonka.Pipes.Types.SelfManagedKafkaAccessConfigurationVpc
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.SelfManagedKafkaAccessConfigurationVpc where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This structure specifies the VPC subnets and security groups for the
-- stream, and whether a public IP address is to be used.
--
-- /See:/ 'newSelfManagedKafkaAccessConfigurationVpc' smart constructor.
data SelfManagedKafkaAccessConfigurationVpc = SelfManagedKafkaAccessConfigurationVpc'
  { -- | Specifies the security groups associated with the stream. These security
    -- groups must all be in the same VPC. You can specify as many as five
    -- security groups. If you do not specify a security group, the default
    -- security group for the VPC is used.
    securityGroup :: Prelude.Maybe [Data.Sensitive Prelude.Text],
    -- | Specifies the subnets associated with the stream. These subnets must all
    -- be in the same VPC. You can specify as many as 16 subnets.
    subnets :: Prelude.Maybe [Data.Sensitive Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SelfManagedKafkaAccessConfigurationVpc' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroup', 'selfManagedKafkaAccessConfigurationVpc_securityGroup' - Specifies the security groups associated with the stream. These security
-- groups must all be in the same VPC. You can specify as many as five
-- security groups. If you do not specify a security group, the default
-- security group for the VPC is used.
--
-- 'subnets', 'selfManagedKafkaAccessConfigurationVpc_subnets' - Specifies the subnets associated with the stream. These subnets must all
-- be in the same VPC. You can specify as many as 16 subnets.
newSelfManagedKafkaAccessConfigurationVpc ::
  SelfManagedKafkaAccessConfigurationVpc
newSelfManagedKafkaAccessConfigurationVpc =
  SelfManagedKafkaAccessConfigurationVpc'
    { securityGroup =
        Prelude.Nothing,
      subnets = Prelude.Nothing
    }

-- | Specifies the security groups associated with the stream. These security
-- groups must all be in the same VPC. You can specify as many as five
-- security groups. If you do not specify a security group, the default
-- security group for the VPC is used.
selfManagedKafkaAccessConfigurationVpc_securityGroup :: Lens.Lens' SelfManagedKafkaAccessConfigurationVpc (Prelude.Maybe [Prelude.Text])
selfManagedKafkaAccessConfigurationVpc_securityGroup = Lens.lens (\SelfManagedKafkaAccessConfigurationVpc' {securityGroup} -> securityGroup) (\s@SelfManagedKafkaAccessConfigurationVpc' {} a -> s {securityGroup = a} :: SelfManagedKafkaAccessConfigurationVpc) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the subnets associated with the stream. These subnets must all
-- be in the same VPC. You can specify as many as 16 subnets.
selfManagedKafkaAccessConfigurationVpc_subnets :: Lens.Lens' SelfManagedKafkaAccessConfigurationVpc (Prelude.Maybe [Prelude.Text])
selfManagedKafkaAccessConfigurationVpc_subnets = Lens.lens (\SelfManagedKafkaAccessConfigurationVpc' {subnets} -> subnets) (\s@SelfManagedKafkaAccessConfigurationVpc' {} a -> s {subnets = a} :: SelfManagedKafkaAccessConfigurationVpc) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    SelfManagedKafkaAccessConfigurationVpc
  where
  parseJSON =
    Data.withObject
      "SelfManagedKafkaAccessConfigurationVpc"
      ( \x ->
          SelfManagedKafkaAccessConfigurationVpc'
            Prelude.<$> (x Data..:? "SecurityGroup" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Subnets" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    SelfManagedKafkaAccessConfigurationVpc
  where
  hashWithSalt
    _salt
    SelfManagedKafkaAccessConfigurationVpc' {..} =
      _salt
        `Prelude.hashWithSalt` securityGroup
        `Prelude.hashWithSalt` subnets

instance
  Prelude.NFData
    SelfManagedKafkaAccessConfigurationVpc
  where
  rnf SelfManagedKafkaAccessConfigurationVpc' {..} =
    Prelude.rnf securityGroup `Prelude.seq`
      Prelude.rnf subnets

instance
  Data.ToJSON
    SelfManagedKafkaAccessConfigurationVpc
  where
  toJSON SelfManagedKafkaAccessConfigurationVpc' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SecurityGroup" Data..=) Prelude.<$> securityGroup,
            ("Subnets" Data..=) Prelude.<$> subnets
          ]
      )
