{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SSM.Types.CommandFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.CommandFilter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.CommandFilterKey

-- | Describes a command filter.
--
-- An instance ID can\'t be specified when a command status is @Pending@
-- because the command hasn\'t run on the instance yet.
--
-- /See:/ 'newCommandFilter' smart constructor.
data CommandFilter = CommandFilter'
  { -- | The name of the filter.
    key :: CommandFilterKey,
    -- | The filter value. Valid values for each filter key are as follows:
    --
    -- -   __InvokedAfter__: Specify a timestamp to limit your results. For
    --     example, specify @2018-07-07T00:00:00Z@ to see a list of command
    --     executions occurring July 7, 2018, and later.
    --
    -- -   __InvokedBefore__: Specify a timestamp to limit your results. For
    --     example, specify @2018-07-07T00:00:00Z@ to see a list of command
    --     executions from before July 7, 2018.
    --
    -- -   __Status__: Specify a valid command status to see a list of all
    --     command executions with that status. Status values you can specify
    --     include:
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
    --     -   @TimedOut@
    --
    --     -   @Cancelling@
    --
    -- -   __DocumentName__: Specify name of the SSM document for which you
    --     want to see command execution results. For example, specify
    --     @AWS-RunPatchBaseline@ to see command executions that used this SSM
    --     document to perform security patching operations on instances.
    --
    -- -   __ExecutionStage__: Specify one of the following values:
    --
    --     -   @Executing@: Returns a list of command executions that are
    --         currently still running.
    --
    --     -   @Complete@: Returns a list of command executions that have
    --         already completed.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'value', 'commandFilter_value' - The filter value. Valid values for each filter key are as follows:
--
-- -   __InvokedAfter__: Specify a timestamp to limit your results. For
--     example, specify @2018-07-07T00:00:00Z@ to see a list of command
--     executions occurring July 7, 2018, and later.
--
-- -   __InvokedBefore__: Specify a timestamp to limit your results. For
--     example, specify @2018-07-07T00:00:00Z@ to see a list of command
--     executions from before July 7, 2018.
--
-- -   __Status__: Specify a valid command status to see a list of all
--     command executions with that status. Status values you can specify
--     include:
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
--     -   @TimedOut@
--
--     -   @Cancelling@
--
-- -   __DocumentName__: Specify name of the SSM document for which you
--     want to see command execution results. For example, specify
--     @AWS-RunPatchBaseline@ to see command executions that used this SSM
--     document to perform security patching operations on instances.
--
-- -   __ExecutionStage__: Specify one of the following values:
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
commandFilter_key :: Lens.Lens' CommandFilter CommandFilterKey
commandFilter_key = Lens.lens (\CommandFilter' {key} -> key) (\s@CommandFilter' {} a -> s {key = a} :: CommandFilter)

-- | The filter value. Valid values for each filter key are as follows:
--
-- -   __InvokedAfter__: Specify a timestamp to limit your results. For
--     example, specify @2018-07-07T00:00:00Z@ to see a list of command
--     executions occurring July 7, 2018, and later.
--
-- -   __InvokedBefore__: Specify a timestamp to limit your results. For
--     example, specify @2018-07-07T00:00:00Z@ to see a list of command
--     executions from before July 7, 2018.
--
-- -   __Status__: Specify a valid command status to see a list of all
--     command executions with that status. Status values you can specify
--     include:
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
--     -   @TimedOut@
--
--     -   @Cancelling@
--
-- -   __DocumentName__: Specify name of the SSM document for which you
--     want to see command execution results. For example, specify
--     @AWS-RunPatchBaseline@ to see command executions that used this SSM
--     document to perform security patching operations on instances.
--
-- -   __ExecutionStage__: Specify one of the following values:
--
--     -   @Executing@: Returns a list of command executions that are
--         currently still running.
--
--     -   @Complete@: Returns a list of command executions that have
--         already completed.
commandFilter_value :: Lens.Lens' CommandFilter Prelude.Text
commandFilter_value = Lens.lens (\CommandFilter' {value} -> value) (\s@CommandFilter' {} a -> s {value = a} :: CommandFilter)

instance Prelude.Hashable CommandFilter

instance Prelude.NFData CommandFilter

instance Prelude.ToJSON CommandFilter where
  toJSON CommandFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("key" Prelude..= key),
            Prelude.Just ("value" Prelude..= value)
          ]
      )
