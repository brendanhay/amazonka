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
-- Module      : Amazonka.ChimeSDKMessaging.Types.Processor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKMessaging.Types.Processor where

import Amazonka.ChimeSDKMessaging.Types.FallbackAction
import Amazonka.ChimeSDKMessaging.Types.ProcessorConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The information about a processor in a channel flow.
--
-- /See:/ 'newProcessor' smart constructor.
data Processor = Processor'
  { -- | The name of the channel flow.
    name :: Data.Sensitive Prelude.Text,
    -- | The information about the type of processor and its identifier.
    configuration :: ProcessorConfiguration,
    -- | The sequence in which processors run. If you have multiple processors in
    -- a channel flow, message processing goes through each processor in the
    -- sequence. The value determines the sequence. At this point, we support
    -- only 1 processor within a flow.
    executionOrder :: Prelude.Natural,
    -- | Determines whether to continue with message processing or stop it in
    -- cases where communication with a processor fails. If a processor has a
    -- fallback action of @ABORT@ and communication with it fails, the
    -- processor sets the message status to @FAILED@ and does not send the
    -- message to any recipients. Note that if the last processor in the
    -- channel flow sequence has a fallback action of @CONTINUE@ and
    -- communication with the processor fails, then the message is considered
    -- processed and sent to recipients of the channel.
    fallbackAction :: FallbackAction
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Processor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'processor_name' - The name of the channel flow.
--
-- 'configuration', 'processor_configuration' - The information about the type of processor and its identifier.
--
-- 'executionOrder', 'processor_executionOrder' - The sequence in which processors run. If you have multiple processors in
-- a channel flow, message processing goes through each processor in the
-- sequence. The value determines the sequence. At this point, we support
-- only 1 processor within a flow.
--
-- 'fallbackAction', 'processor_fallbackAction' - Determines whether to continue with message processing or stop it in
-- cases where communication with a processor fails. If a processor has a
-- fallback action of @ABORT@ and communication with it fails, the
-- processor sets the message status to @FAILED@ and does not send the
-- message to any recipients. Note that if the last processor in the
-- channel flow sequence has a fallback action of @CONTINUE@ and
-- communication with the processor fails, then the message is considered
-- processed and sent to recipients of the channel.
newProcessor ::
  -- | 'name'
  Prelude.Text ->
  -- | 'configuration'
  ProcessorConfiguration ->
  -- | 'executionOrder'
  Prelude.Natural ->
  -- | 'fallbackAction'
  FallbackAction ->
  Processor
newProcessor
  pName_
  pConfiguration_
  pExecutionOrder_
  pFallbackAction_ =
    Processor'
      { name = Data._Sensitive Lens.# pName_,
        configuration = pConfiguration_,
        executionOrder = pExecutionOrder_,
        fallbackAction = pFallbackAction_
      }

-- | The name of the channel flow.
processor_name :: Lens.Lens' Processor Prelude.Text
processor_name = Lens.lens (\Processor' {name} -> name) (\s@Processor' {} a -> s {name = a} :: Processor) Prelude.. Data._Sensitive

-- | The information about the type of processor and its identifier.
processor_configuration :: Lens.Lens' Processor ProcessorConfiguration
processor_configuration = Lens.lens (\Processor' {configuration} -> configuration) (\s@Processor' {} a -> s {configuration = a} :: Processor)

-- | The sequence in which processors run. If you have multiple processors in
-- a channel flow, message processing goes through each processor in the
-- sequence. The value determines the sequence. At this point, we support
-- only 1 processor within a flow.
processor_executionOrder :: Lens.Lens' Processor Prelude.Natural
processor_executionOrder = Lens.lens (\Processor' {executionOrder} -> executionOrder) (\s@Processor' {} a -> s {executionOrder = a} :: Processor)

-- | Determines whether to continue with message processing or stop it in
-- cases where communication with a processor fails. If a processor has a
-- fallback action of @ABORT@ and communication with it fails, the
-- processor sets the message status to @FAILED@ and does not send the
-- message to any recipients. Note that if the last processor in the
-- channel flow sequence has a fallback action of @CONTINUE@ and
-- communication with the processor fails, then the message is considered
-- processed and sent to recipients of the channel.
processor_fallbackAction :: Lens.Lens' Processor FallbackAction
processor_fallbackAction = Lens.lens (\Processor' {fallbackAction} -> fallbackAction) (\s@Processor' {} a -> s {fallbackAction = a} :: Processor)

instance Data.FromJSON Processor where
  parseJSON =
    Data.withObject
      "Processor"
      ( \x ->
          Processor'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "Configuration")
            Prelude.<*> (x Data..: "ExecutionOrder")
            Prelude.<*> (x Data..: "FallbackAction")
      )

instance Prelude.Hashable Processor where
  hashWithSalt _salt Processor' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` executionOrder
      `Prelude.hashWithSalt` fallbackAction

instance Prelude.NFData Processor where
  rnf Processor' {..} =
    Prelude.rnf name `Prelude.seq`
      Prelude.rnf configuration `Prelude.seq`
        Prelude.rnf executionOrder `Prelude.seq`
          Prelude.rnf fallbackAction

instance Data.ToJSON Processor where
  toJSON Processor' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Configuration" Data..= configuration),
            Prelude.Just
              ("ExecutionOrder" Data..= executionOrder),
            Prelude.Just
              ("FallbackAction" Data..= fallbackAction)
          ]
      )
