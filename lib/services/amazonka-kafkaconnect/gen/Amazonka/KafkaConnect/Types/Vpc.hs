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
-- Module      : Amazonka.KafkaConnect.Types.Vpc
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.Vpc where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the VPC in which the connector resides.
--
-- /See:/ 'newVpc' smart constructor.
data Vpc = Vpc'
  { -- | The security groups for the connector.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The subnets for the connector.
    subnets :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Vpc' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroups', 'vpc_securityGroups' - The security groups for the connector.
--
-- 'subnets', 'vpc_subnets' - The subnets for the connector.
newVpc ::
  Vpc
newVpc =
  Vpc'
    { securityGroups = Prelude.Nothing,
      subnets = Prelude.mempty
    }

-- | The security groups for the connector.
vpc_securityGroups :: Lens.Lens' Vpc (Prelude.Maybe [Prelude.Text])
vpc_securityGroups = Lens.lens (\Vpc' {securityGroups} -> securityGroups) (\s@Vpc' {} a -> s {securityGroups = a} :: Vpc) Prelude.. Lens.mapping Lens.coerced

-- | The subnets for the connector.
vpc_subnets :: Lens.Lens' Vpc [Prelude.Text]
vpc_subnets = Lens.lens (\Vpc' {subnets} -> subnets) (\s@Vpc' {} a -> s {subnets = a} :: Vpc) Prelude.. Lens.coerced

instance Prelude.Hashable Vpc where
  hashWithSalt _salt Vpc' {..} =
    _salt `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` subnets

instance Prelude.NFData Vpc where
  rnf Vpc' {..} =
    Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf subnets

instance Data.ToJSON Vpc where
  toJSON Vpc' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("securityGroups" Data..=)
              Prelude.<$> securityGroups,
            Prelude.Just ("subnets" Data..= subnets)
          ]
      )
