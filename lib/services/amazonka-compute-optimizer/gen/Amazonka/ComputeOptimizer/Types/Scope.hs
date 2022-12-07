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
-- Module      : Amazonka.ComputeOptimizer.Types.Scope
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.Scope where

import Amazonka.ComputeOptimizer.Types.ScopeName
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the scope of a recommendation preference.
--
-- Recommendation preferences can be created at the organization level (for
-- management accounts of an organization only), account level, and
-- resource level. For more information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/enhanced-infrastructure-metrics.html Activating enhanced infrastructure metrics>
-- in the /Compute Optimizer User Guide/.
--
-- You cannot create recommendation preferences for Auto Scaling groups at
-- the organization and account levels. You can create recommendation
-- preferences for Auto Scaling groups only at the resource level by
-- specifying a scope name of @ResourceArn@ and a scope value of the Auto
-- Scaling group Amazon Resource Name (ARN). This will configure the
-- preference for all instances that are part of the specified Auto Scaling
-- group. You also cannot create recommendation preferences at the resource
-- level for instances that are part of an Auto Scaling group. You can
-- create recommendation preferences at the resource level only for
-- standalone instances.
--
-- /See:/ 'newScope' smart constructor.
data Scope = Scope'
  { -- | The name of the scope.
    --
    -- The following scopes are possible:
    --
    -- -   @Organization@ - Specifies that the recommendation preference
    --     applies at the organization level, for all member accounts of an
    --     organization.
    --
    -- -   @AccountId@ - Specifies that the recommendation preference applies
    --     at the account level, for all resources of a given resource type in
    --     an account.
    --
    -- -   @ResourceArn@ - Specifies that the recommendation preference applies
    --     at the individual resource level.
    name :: Prelude.Maybe ScopeName,
    -- | The value of the scope.
    --
    -- If you specified the @name@ of the scope as:
    --
    -- -   @Organization@ - The @value@ must be @ALL_ACCOUNTS@.
    --
    -- -   @AccountId@ - The @value@ must be a 12-digit Amazon Web Services
    --     account ID.
    --
    -- -   @ResourceArn@ - The @value@ must be the Amazon Resource Name (ARN)
    --     of an EC2 instance or an Auto Scaling group.
    --
    -- Only EC2 instance and Auto Scaling group ARNs are currently supported.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Scope' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'scope_name' - The name of the scope.
--
-- The following scopes are possible:
--
-- -   @Organization@ - Specifies that the recommendation preference
--     applies at the organization level, for all member accounts of an
--     organization.
--
-- -   @AccountId@ - Specifies that the recommendation preference applies
--     at the account level, for all resources of a given resource type in
--     an account.
--
-- -   @ResourceArn@ - Specifies that the recommendation preference applies
--     at the individual resource level.
--
-- 'value', 'scope_value' - The value of the scope.
--
-- If you specified the @name@ of the scope as:
--
-- -   @Organization@ - The @value@ must be @ALL_ACCOUNTS@.
--
-- -   @AccountId@ - The @value@ must be a 12-digit Amazon Web Services
--     account ID.
--
-- -   @ResourceArn@ - The @value@ must be the Amazon Resource Name (ARN)
--     of an EC2 instance or an Auto Scaling group.
--
-- Only EC2 instance and Auto Scaling group ARNs are currently supported.
newScope ::
  Scope
newScope =
  Scope'
    { name = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the scope.
--
-- The following scopes are possible:
--
-- -   @Organization@ - Specifies that the recommendation preference
--     applies at the organization level, for all member accounts of an
--     organization.
--
-- -   @AccountId@ - Specifies that the recommendation preference applies
--     at the account level, for all resources of a given resource type in
--     an account.
--
-- -   @ResourceArn@ - Specifies that the recommendation preference applies
--     at the individual resource level.
scope_name :: Lens.Lens' Scope (Prelude.Maybe ScopeName)
scope_name = Lens.lens (\Scope' {name} -> name) (\s@Scope' {} a -> s {name = a} :: Scope)

-- | The value of the scope.
--
-- If you specified the @name@ of the scope as:
--
-- -   @Organization@ - The @value@ must be @ALL_ACCOUNTS@.
--
-- -   @AccountId@ - The @value@ must be a 12-digit Amazon Web Services
--     account ID.
--
-- -   @ResourceArn@ - The @value@ must be the Amazon Resource Name (ARN)
--     of an EC2 instance or an Auto Scaling group.
--
-- Only EC2 instance and Auto Scaling group ARNs are currently supported.
scope_value :: Lens.Lens' Scope (Prelude.Maybe Prelude.Text)
scope_value = Lens.lens (\Scope' {value} -> value) (\s@Scope' {} a -> s {value = a} :: Scope)

instance Data.FromJSON Scope where
  parseJSON =
    Data.withObject
      "Scope"
      ( \x ->
          Scope'
            Prelude.<$> (x Data..:? "name") Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable Scope where
  hashWithSalt _salt Scope' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData Scope where
  rnf Scope' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

instance Data.ToJSON Scope where
  toJSON Scope' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("value" Data..=) Prelude.<$> value
          ]
      )
