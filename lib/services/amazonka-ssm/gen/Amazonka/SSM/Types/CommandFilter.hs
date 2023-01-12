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
-- Module      : Amazonka.SSM.Types.CommandFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.CommandFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.CommandFilterKey

-- | Describes a command filter.
--
-- A managed node ID can\'t be specified when a command status is @Pending@
-- because the command hasn\'t run on the node yet.
--
-- /See:/ 'newCommandFilter' smart constructor.
data CommandFilter = CommandFilter'
  { -- | The name of the filter.
    --
    -- The @ExecutionStage@ filter can\'t be used with the
    -- @ListCommandInvocations@ operation, only with @ListCommands@.
    key :: CommandFilterKey,
    -- | The filter value. Valid values for each filter key are as follows:
    --
    -- -   __InvokedAfter__: Specify a timestamp to limit your results. For
    --     example, specify @2021-07-07T00:00:00Z@ to see a list of command
    --     executions occurring July 7, 2021, and later.
    --
    -- -   __InvokedBefore__: Specify a timestamp to limit your results. For
    --     example, specify @2021-07-07T00:00:00Z@ to see a list of command
    --     executions from before July 7, 2021.
    --
    -- -   __Status__: Specify a valid command status to see a list of all
    --     command executions with that status. The status choices depend on
    --     the API you call.
    --
    --     The status values you can specify for @ListCommands@ are:
    --
    --     -   @Pending@
    --
    --     -   @InProgress@
    --
    --     -   @Success@
    --
    --     -   @Cancelled@
    --
    --     -   @Failed@
    --
    --     -   @TimedOut@ (this includes both Delivery and Execution time outs)
    --
    --     -   @AccessDenied@
    --
    --     -   @DeliveryTimedOut@
    --
    --     -   @ExecutionTimedOut@
    --
    --     -   @Incomplete@
    --
    --     -   @NoInstancesInTag@
    --
    --     -   @LimitExceeded@
    --
    --     The status values you can specify for @ListCommandInvocations@ are:
    --
    --     -   @Pending@
    --
    --     -   @InProgress@
    --
    --     -   @Delayed@
    --
    --     -   @Success@
    --
    --     -   @Cancelled@
    --
    --     -   @Failed@
    --
    --     -   @TimedOut@ (this includes both Delivery and Execution time outs)
    --
    --     -   @AccessDenied@
    --
    --     -   @DeliveryTimedOut@
    --
    --     -   @ExecutionTimedOut@
    --
    --     -   @Undeliverable@
    --
    --     -   @InvalidPlatform@
    --
    --     -   @Terminated@
    --
    -- -   __DocumentName__: Specify name of the Amazon Web Services Systems
    --     Manager document (SSM document) for which you want to see command
    --     execution results. For example, specify @AWS-RunPatchBaseline@ to
    --     see command executions that used this SSM document to perform
    --     security patching operations on managed nodes.
    --
    -- -   __ExecutionStage__: Specify one of the following values
    --     (@ListCommands@ operations only):
    --
    --     -   @Executing@: Returns a list of command executions that are
    --         currently still running.
    --
    --     -   @Complete@: Returns a list of command executions that have
    --         already completed.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CommandFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'commandFilter_key' - The name of the filter.
--
-- The @ExecutionStage@ filter can\'t be used with the
-- @ListCommandInvocations@ operation, only with @ListCommands@.
--
-- 'value', 'commandFilter_value' - The filter value. Valid values for each filter key are as follows:
--
-- -   __InvokedAfter__: Specify a timestamp to limit your results. For
--     example, specify @2021-07-07T00:00:00Z@ to see a list of command
--     executions occurring July 7, 2021, and later.
--
-- -   __InvokedBefore__: Specify a timestamp to limit your results. For
--     example, specify @2021-07-07T00:00:00Z@ to see a list of command
--     executions from before July 7, 2021.
--
-- -   __Status__: Specify a valid command status to see a list of all
--     command executions with that status. The status choices depend on
--     the API you call.
--
--     The status values you can specify for @ListCommands@ are:
--
--     -   @Pending@
--
--     -   @InProgress@
--
--     -   @Success@
--
--     -   @Cancelled@
--
--     -   @Failed@
--
--     -   @TimedOut@ (this includes both Delivery and Execution time outs)
--
--     -   @AccessDenied@
--
--     -   @DeliveryTimedOut@
--
--     -   @ExecutionTimedOut@
--
--     -   @Incomplete@
--
--     -   @NoInstancesInTag@
--
--     -   @LimitExceeded@
--
--     The status values you can specify for @ListCommandInvocations@ are:
--
--     -   @Pending@
--
--     -   @InProgress@
--
--     -   @Delayed@
--
--     -   @Success@
--
--     -   @Cancelled@
--
--     -   @Failed@
--
--     -   @TimedOut@ (this includes both Delivery and Execution time outs)
--
--     -   @AccessDenied@
--
--     -   @DeliveryTimedOut@
--
--     -   @ExecutionTimedOut@
--
--     -   @Undeliverable@
--
--     -   @InvalidPlatform@
--
--     -   @Terminated@
--
-- -   __DocumentName__: Specify name of the Amazon Web Services Systems
--     Manager document (SSM document) for which you want to see command
--     execution results. For example, specify @AWS-RunPatchBaseline@ to
--     see command executions that used this SSM document to perform
--     security patching operations on managed nodes.
--
-- -   __ExecutionStage__: Specify one of the following values
--     (@ListCommands@ operations only):
--
--     -   @Executing@: Returns a list of command executions that are
--         currently still running.
--
--     -   @Complete@: Returns a list of command executions that have
--         already completed.
newCommandFilter ::
  -- | 'key'
  CommandFilterKey ->
  -- | 'value'
  Prelude.Text ->
  CommandFilter
newCommandFilter pKey_ pValue_ =
  CommandFilter' {key = pKey_, value = pValue_}

-- | The name of the filter.
--
-- The @ExecutionStage@ filter can\'t be used with the
-- @ListCommandInvocations@ operation, only with @ListCommands@.
commandFilter_key :: Lens.Lens' CommandFilter CommandFilterKey
commandFilter_key = Lens.lens (\CommandFilter' {key} -> key) (\s@CommandFilter' {} a -> s {key = a} :: CommandFilter)

-- | The filter value. Valid values for each filter key are as follows:
--
-- -   __InvokedAfter__: Specify a timestamp to limit your results. For
--     example, specify @2021-07-07T00:00:00Z@ to see a list of command
--     executions occurring July 7, 2021, and later.
--
-- -   __InvokedBefore__: Specify a timestamp to limit your results. For
--     example, specify @2021-07-07T00:00:00Z@ to see a list of command
--     executions from before July 7, 2021.
--
-- -   __Status__: Specify a valid command status to see a list of all
--     command executions with that status. The status choices depend on
--     the API you call.
--
--     The status values you can specify for @ListCommands@ are:
--
--     -   @Pending@
--
--     -   @InProgress@
--
--     -   @Success@
--
--     -   @Cancelled@
--
--     -   @Failed@
--
--     -   @TimedOut@ (this includes both Delivery and Execution time outs)
--
--     -   @AccessDenied@
--
--     -   @DeliveryTimedOut@
--
--     -   @ExecutionTimedOut@
--
--     -   @Incomplete@
--
--     -   @NoInstancesInTag@
--
--     -   @LimitExceeded@
--
--     The status values you can specify for @ListCommandInvocations@ are:
--
--     -   @Pending@
--
--     -   @InProgress@
--
--     -   @Delayed@
--
--     -   @Success@
--
--     -   @Cancelled@
--
--     -   @Failed@
--
--     -   @TimedOut@ (this includes both Delivery and Execution time outs)
--
--     -   @AccessDenied@
--
--     -   @DeliveryTimedOut@
--
--     -   @ExecutionTimedOut@
--
--     -   @Undeliverable@
--
--     -   @InvalidPlatform@
--
--     -   @Terminated@
--
-- -   __DocumentName__: Specify name of the Amazon Web Services Systems
--     Manager document (SSM document) for which you want to see command
--     execution results. For example, specify @AWS-RunPatchBaseline@ to
--     see command executions that used this SSM document to perform
--     security patching operations on managed nodes.
--
-- -   __ExecutionStage__: Specify one of the following values
--     (@ListCommands@ operations only):
--
--     -   @Executing@: Returns a list of command executions that are
--         currently still running.
--
--     -   @Complete@: Returns a list of command executions that have
--         already completed.
commandFilter_value :: Lens.Lens' CommandFilter Prelude.Text
commandFilter_value = Lens.lens (\CommandFilter' {value} -> value) (\s@CommandFilter' {} a -> s {value = a} :: CommandFilter)

instance Prelude.Hashable CommandFilter where
  hashWithSalt _salt CommandFilter' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData CommandFilter where
  rnf CommandFilter' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value

instance Data.ToJSON CommandFilter where
  toJSON CommandFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("key" Data..= key),
            Prelude.Just ("value" Data..= value)
          ]
      )
