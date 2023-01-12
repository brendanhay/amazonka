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
-- Module      : Amazonka.SSM.Types.Target
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.Target where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An array of search criteria that targets managed nodes using a key-value
-- pair that you specify.
--
-- One or more targets must be specified for maintenance window Run
-- Command-type tasks. Depending on the task, targets are optional for
-- other maintenance window task types (Automation, Lambda, and Step
-- Functions). For more information about running tasks that don\'t specify
-- targets, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/maintenance-windows-targetless-tasks.html Registering maintenance window tasks without targets>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- Supported formats include the following.
--
-- -   @Key=InstanceIds,Values=\<instance-id-1>,\<instance-id-2>,\<instance-id-3>@
--
-- -   @Key=tag:\<my-tag-key>,Values=\<my-tag-value-1>,\<my-tag-value-2>@
--
-- -   @Key=tag-key,Values=\<my-tag-key-1>,\<my-tag-key-2>@
--
-- -   __Run Command and Maintenance window targets only__:
--     @Key=resource-groups:Name,Values=\<resource-group-name>@
--
-- -   __Maintenance window targets only__:
--     @Key=resource-groups:ResourceTypeFilters,Values=\<resource-type-1>,\<resource-type-2>@
--
-- -   __Automation targets only__:
--     @Key=ResourceGroup;Values=\<resource-group-name>@
--
-- For example:
--
-- -   @Key=InstanceIds,Values=i-02573cafcfEXAMPLE,i-0471e04240EXAMPLE,i-07782c72faEXAMPLE@
--
-- -   @Key=tag:CostCenter,Values=CostCenter1,CostCenter2,CostCenter3@
--
-- -   @Key=tag-key,Values=Name,Instance-Type,CostCenter@
--
-- -   __Run Command and Maintenance window targets only__:
--     @Key=resource-groups:Name,Values=ProductionResourceGroup@
--
--     This example demonstrates how to target all resources in the
--     resource group __ProductionResourceGroup__ in your maintenance
--     window.
--
-- -   __Maintenance window targets only__:
--     @Key=resource-groups:ResourceTypeFilters,Values=AWS::EC2::INSTANCE,AWS::EC2::VPC@
--
--     This example demonstrates how to target only Amazon Elastic Compute
--     Cloud (Amazon EC2) instances and VPCs in your maintenance window.
--
-- -   __Automation targets only__:
--     @Key=ResourceGroup,Values=MyResourceGroup@
--
-- -   __State Manager association targets only__:
--     @Key=InstanceIds,Values=*@
--
--     This example demonstrates how to target all managed instances in the
--     Amazon Web Services Region where the association was created.
--
-- For more information about how to send commands that target managed
-- nodes using @Key,Value@ parameters, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html#send-commands-targeting Targeting multiple instances>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- /See:/ 'newTarget' smart constructor.
data Target = Target'
  { -- | User-defined criteria for sending commands that target managed nodes
    -- that meet the criteria.
    key :: Prelude.Maybe Prelude.Text,
    -- | User-defined criteria that maps to @Key@. For example, if you specified
    -- @tag:ServerRole@, you could specify @value:WebServer@ to run a command
    -- on instances that include EC2 tags of @ServerRole,WebServer@.
    --
    -- Depending on the type of target, the maximum number of values for a key
    -- might be lower than the global maximum of 50.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Target' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'target_key' - User-defined criteria for sending commands that target managed nodes
-- that meet the criteria.
--
-- 'values', 'target_values' - User-defined criteria that maps to @Key@. For example, if you specified
-- @tag:ServerRole@, you could specify @value:WebServer@ to run a command
-- on instances that include EC2 tags of @ServerRole,WebServer@.
--
-- Depending on the type of target, the maximum number of values for a key
-- might be lower than the global maximum of 50.
newTarget ::
  Target
newTarget =
  Target'
    { key = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | User-defined criteria for sending commands that target managed nodes
-- that meet the criteria.
target_key :: Lens.Lens' Target (Prelude.Maybe Prelude.Text)
target_key = Lens.lens (\Target' {key} -> key) (\s@Target' {} a -> s {key = a} :: Target)

-- | User-defined criteria that maps to @Key@. For example, if you specified
-- @tag:ServerRole@, you could specify @value:WebServer@ to run a command
-- on instances that include EC2 tags of @ServerRole,WebServer@.
--
-- Depending on the type of target, the maximum number of values for a key
-- might be lower than the global maximum of 50.
target_values :: Lens.Lens' Target (Prelude.Maybe [Prelude.Text])
target_values = Lens.lens (\Target' {values} -> values) (\s@Target' {} a -> s {values = a} :: Target) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Target where
  parseJSON =
    Data.withObject
      "Target"
      ( \x ->
          Target'
            Prelude.<$> (x Data..:? "Key")
            Prelude.<*> (x Data..:? "Values" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Target where
  hashWithSalt _salt Target' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` values

instance Prelude.NFData Target where
  rnf Target' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf values

instance Data.ToJSON Target where
  toJSON Target' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Key" Data..=) Prelude.<$> key,
            ("Values" Data..=) Prelude.<$> values
          ]
      )
