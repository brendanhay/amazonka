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
-- Module      : Amazonka.SageMakerA2IRuntime.Types.HumanLoopSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerA2IRuntime.Types.HumanLoopSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerA2IRuntime.Types.HumanLoopStatus

-- | Summary information about the human loop.
--
-- /See:/ 'newHumanLoopSummary' smart constructor.
data HumanLoopSummary = HumanLoopSummary'
  { -- | When Amazon Augmented AI created the human loop.
    creationTime :: Prelude.Maybe Data.ISO8601,
    -- | The reason why the human loop failed. A failure reason is returned when
    -- the status of the human loop is @Failed@.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the flow definition used to configure
    -- the human loop.
    flowDefinitionArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the human loop.
    humanLoopName :: Prelude.Maybe Prelude.Text,
    -- | The status of the human loop.
    humanLoopStatus :: Prelude.Maybe HumanLoopStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HumanLoopSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'humanLoopSummary_creationTime' - When Amazon Augmented AI created the human loop.
--
-- 'failureReason', 'humanLoopSummary_failureReason' - The reason why the human loop failed. A failure reason is returned when
-- the status of the human loop is @Failed@.
--
-- 'flowDefinitionArn', 'humanLoopSummary_flowDefinitionArn' - The Amazon Resource Name (ARN) of the flow definition used to configure
-- the human loop.
--
-- 'humanLoopName', 'humanLoopSummary_humanLoopName' - The name of the human loop.
--
-- 'humanLoopStatus', 'humanLoopSummary_humanLoopStatus' - The status of the human loop.
newHumanLoopSummary ::
  HumanLoopSummary
newHumanLoopSummary =
  HumanLoopSummary'
    { creationTime = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      flowDefinitionArn = Prelude.Nothing,
      humanLoopName = Prelude.Nothing,
      humanLoopStatus = Prelude.Nothing
    }

-- | When Amazon Augmented AI created the human loop.
humanLoopSummary_creationTime :: Lens.Lens' HumanLoopSummary (Prelude.Maybe Prelude.UTCTime)
humanLoopSummary_creationTime = Lens.lens (\HumanLoopSummary' {creationTime} -> creationTime) (\s@HumanLoopSummary' {} a -> s {creationTime = a} :: HumanLoopSummary) Prelude.. Lens.mapping Data._Time

-- | The reason why the human loop failed. A failure reason is returned when
-- the status of the human loop is @Failed@.
humanLoopSummary_failureReason :: Lens.Lens' HumanLoopSummary (Prelude.Maybe Prelude.Text)
humanLoopSummary_failureReason = Lens.lens (\HumanLoopSummary' {failureReason} -> failureReason) (\s@HumanLoopSummary' {} a -> s {failureReason = a} :: HumanLoopSummary)

-- | The Amazon Resource Name (ARN) of the flow definition used to configure
-- the human loop.
humanLoopSummary_flowDefinitionArn :: Lens.Lens' HumanLoopSummary (Prelude.Maybe Prelude.Text)
humanLoopSummary_flowDefinitionArn = Lens.lens (\HumanLoopSummary' {flowDefinitionArn} -> flowDefinitionArn) (\s@HumanLoopSummary' {} a -> s {flowDefinitionArn = a} :: HumanLoopSummary)

-- | The name of the human loop.
humanLoopSummary_humanLoopName :: Lens.Lens' HumanLoopSummary (Prelude.Maybe Prelude.Text)
humanLoopSummary_humanLoopName = Lens.lens (\HumanLoopSummary' {humanLoopName} -> humanLoopName) (\s@HumanLoopSummary' {} a -> s {humanLoopName = a} :: HumanLoopSummary)

-- | The status of the human loop.
humanLoopSummary_humanLoopStatus :: Lens.Lens' HumanLoopSummary (Prelude.Maybe HumanLoopStatus)
humanLoopSummary_humanLoopStatus = Lens.lens (\HumanLoopSummary' {humanLoopStatus} -> humanLoopStatus) (\s@HumanLoopSummary' {} a -> s {humanLoopStatus = a} :: HumanLoopSummary)

instance Data.FromJSON HumanLoopSummary where
  parseJSON =
    Data.withObject
      "HumanLoopSummary"
      ( \x ->
          HumanLoopSummary'
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "FailureReason")
            Prelude.<*> (x Data..:? "FlowDefinitionArn")
            Prelude.<*> (x Data..:? "HumanLoopName")
            Prelude.<*> (x Data..:? "HumanLoopStatus")
      )

instance Prelude.Hashable HumanLoopSummary where
  hashWithSalt _salt HumanLoopSummary' {..} =
    _salt `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` flowDefinitionArn
      `Prelude.hashWithSalt` humanLoopName
      `Prelude.hashWithSalt` humanLoopStatus

instance Prelude.NFData HumanLoopSummary where
  rnf HumanLoopSummary' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf flowDefinitionArn
      `Prelude.seq` Prelude.rnf humanLoopName
      `Prelude.seq` Prelude.rnf humanLoopStatus
