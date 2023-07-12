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
-- Module      : Amazonka.Route53.Types.AccountLimit
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Types.AccountLimit where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53.Internal
import Amazonka.Route53.Types.AccountLimitType

-- | A complex type that contains the type of limit that you specified in the
-- request and the current value for that limit.
--
-- /See:/ 'newAccountLimit' smart constructor.
data AccountLimit = AccountLimit'
  { -- | The limit that you requested. Valid values include the following:
    --
    -- -   __MAX_HEALTH_CHECKS_BY_OWNER__: The maximum number of health checks
    --     that you can create using the current account.
    --
    -- -   __MAX_HOSTED_ZONES_BY_OWNER__: The maximum number of hosted zones
    --     that you can create using the current account.
    --
    -- -   __MAX_REUSABLE_DELEGATION_SETS_BY_OWNER__: The maximum number of
    --     reusable delegation sets that you can create using the current
    --     account.
    --
    -- -   __MAX_TRAFFIC_POLICIES_BY_OWNER__: The maximum number of traffic
    --     policies that you can create using the current account.
    --
    -- -   __MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER__: The maximum number of
    --     traffic policy instances that you can create using the current
    --     account. (Traffic policy instances are referred to as traffic flow
    --     policy records in the Amazon Route 53 console.)
    type' :: AccountLimitType,
    -- | The current value for the limit that is specified by
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_AccountLimit.html#Route53-Type-AccountLimit-Type Type>.
    value :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountLimit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'accountLimit_type' - The limit that you requested. Valid values include the following:
--
-- -   __MAX_HEALTH_CHECKS_BY_OWNER__: The maximum number of health checks
--     that you can create using the current account.
--
-- -   __MAX_HOSTED_ZONES_BY_OWNER__: The maximum number of hosted zones
--     that you can create using the current account.
--
-- -   __MAX_REUSABLE_DELEGATION_SETS_BY_OWNER__: The maximum number of
--     reusable delegation sets that you can create using the current
--     account.
--
-- -   __MAX_TRAFFIC_POLICIES_BY_OWNER__: The maximum number of traffic
--     policies that you can create using the current account.
--
-- -   __MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER__: The maximum number of
--     traffic policy instances that you can create using the current
--     account. (Traffic policy instances are referred to as traffic flow
--     policy records in the Amazon Route 53 console.)
--
-- 'value', 'accountLimit_value' - The current value for the limit that is specified by
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_AccountLimit.html#Route53-Type-AccountLimit-Type Type>.
newAccountLimit ::
  -- | 'type''
  AccountLimitType ->
  -- | 'value'
  Prelude.Natural ->
  AccountLimit
newAccountLimit pType_ pValue_ =
  AccountLimit' {type' = pType_, value = pValue_}

-- | The limit that you requested. Valid values include the following:
--
-- -   __MAX_HEALTH_CHECKS_BY_OWNER__: The maximum number of health checks
--     that you can create using the current account.
--
-- -   __MAX_HOSTED_ZONES_BY_OWNER__: The maximum number of hosted zones
--     that you can create using the current account.
--
-- -   __MAX_REUSABLE_DELEGATION_SETS_BY_OWNER__: The maximum number of
--     reusable delegation sets that you can create using the current
--     account.
--
-- -   __MAX_TRAFFIC_POLICIES_BY_OWNER__: The maximum number of traffic
--     policies that you can create using the current account.
--
-- -   __MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER__: The maximum number of
--     traffic policy instances that you can create using the current
--     account. (Traffic policy instances are referred to as traffic flow
--     policy records in the Amazon Route 53 console.)
accountLimit_type :: Lens.Lens' AccountLimit AccountLimitType
accountLimit_type = Lens.lens (\AccountLimit' {type'} -> type') (\s@AccountLimit' {} a -> s {type' = a} :: AccountLimit)

-- | The current value for the limit that is specified by
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_AccountLimit.html#Route53-Type-AccountLimit-Type Type>.
accountLimit_value :: Lens.Lens' AccountLimit Prelude.Natural
accountLimit_value = Lens.lens (\AccountLimit' {value} -> value) (\s@AccountLimit' {} a -> s {value = a} :: AccountLimit)

instance Data.FromXML AccountLimit where
  parseXML x =
    AccountLimit'
      Prelude.<$> (x Data..@ "Type")
      Prelude.<*> (x Data..@ "Value")

instance Prelude.Hashable AccountLimit where
  hashWithSalt _salt AccountLimit' {..} =
    _salt
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` value

instance Prelude.NFData AccountLimit where
  rnf AccountLimit' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf value
