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
-- Module      : Amazonka.MQ.Types.BrokerSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MQ.Types.BrokerSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MQ.Types.BrokerState
import Amazonka.MQ.Types.DeploymentMode
import Amazonka.MQ.Types.EngineType
import qualified Amazonka.Prelude as Prelude

-- | Returns information about all brokers.
--
-- /See:/ 'newBrokerSummary' smart constructor.
data BrokerSummary = BrokerSummary'
  { -- | The broker\'s name. This value is unique in your AWS account, 1-50
    -- characters long, and containing only letters, numbers, dashes, and
    -- underscores, and must not contain white spaces, brackets, wildcard
    -- characters, or special characters.
    brokerName :: Prelude.Maybe Prelude.Text,
    -- | The time when the broker was created.
    created :: Prelude.Maybe Data.POSIX,
    -- | The broker\'s status.
    brokerState :: Prelude.Maybe BrokerState,
    -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Prelude.Maybe Prelude.Text,
    -- | The broker\'s instance type.
    hostInstanceType :: Prelude.Maybe Prelude.Text,
    -- | The broker\'s Amazon Resource Name (ARN).
    brokerArn :: Prelude.Maybe Prelude.Text,
    -- | The broker\'s deployment mode.
    deploymentMode :: DeploymentMode,
    -- | The type of broker engine.
    engineType :: EngineType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BrokerSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'brokerName', 'brokerSummary_brokerName' - The broker\'s name. This value is unique in your AWS account, 1-50
-- characters long, and containing only letters, numbers, dashes, and
-- underscores, and must not contain white spaces, brackets, wildcard
-- characters, or special characters.
--
-- 'created', 'brokerSummary_created' - The time when the broker was created.
--
-- 'brokerState', 'brokerSummary_brokerState' - The broker\'s status.
--
-- 'brokerId', 'brokerSummary_brokerId' - The unique ID that Amazon MQ generates for the broker.
--
-- 'hostInstanceType', 'brokerSummary_hostInstanceType' - The broker\'s instance type.
--
-- 'brokerArn', 'brokerSummary_brokerArn' - The broker\'s Amazon Resource Name (ARN).
--
-- 'deploymentMode', 'brokerSummary_deploymentMode' - The broker\'s deployment mode.
--
-- 'engineType', 'brokerSummary_engineType' - The type of broker engine.
newBrokerSummary ::
  -- | 'deploymentMode'
  DeploymentMode ->
  -- | 'engineType'
  EngineType ->
  BrokerSummary
newBrokerSummary pDeploymentMode_ pEngineType_ =
  BrokerSummary'
    { brokerName = Prelude.Nothing,
      created = Prelude.Nothing,
      brokerState = Prelude.Nothing,
      brokerId = Prelude.Nothing,
      hostInstanceType = Prelude.Nothing,
      brokerArn = Prelude.Nothing,
      deploymentMode = pDeploymentMode_,
      engineType = pEngineType_
    }

-- | The broker\'s name. This value is unique in your AWS account, 1-50
-- characters long, and containing only letters, numbers, dashes, and
-- underscores, and must not contain white spaces, brackets, wildcard
-- characters, or special characters.
brokerSummary_brokerName :: Lens.Lens' BrokerSummary (Prelude.Maybe Prelude.Text)
brokerSummary_brokerName = Lens.lens (\BrokerSummary' {brokerName} -> brokerName) (\s@BrokerSummary' {} a -> s {brokerName = a} :: BrokerSummary)

-- | The time when the broker was created.
brokerSummary_created :: Lens.Lens' BrokerSummary (Prelude.Maybe Prelude.UTCTime)
brokerSummary_created = Lens.lens (\BrokerSummary' {created} -> created) (\s@BrokerSummary' {} a -> s {created = a} :: BrokerSummary) Prelude.. Lens.mapping Data._Time

-- | The broker\'s status.
brokerSummary_brokerState :: Lens.Lens' BrokerSummary (Prelude.Maybe BrokerState)
brokerSummary_brokerState = Lens.lens (\BrokerSummary' {brokerState} -> brokerState) (\s@BrokerSummary' {} a -> s {brokerState = a} :: BrokerSummary)

-- | The unique ID that Amazon MQ generates for the broker.
brokerSummary_brokerId :: Lens.Lens' BrokerSummary (Prelude.Maybe Prelude.Text)
brokerSummary_brokerId = Lens.lens (\BrokerSummary' {brokerId} -> brokerId) (\s@BrokerSummary' {} a -> s {brokerId = a} :: BrokerSummary)

-- | The broker\'s instance type.
brokerSummary_hostInstanceType :: Lens.Lens' BrokerSummary (Prelude.Maybe Prelude.Text)
brokerSummary_hostInstanceType = Lens.lens (\BrokerSummary' {hostInstanceType} -> hostInstanceType) (\s@BrokerSummary' {} a -> s {hostInstanceType = a} :: BrokerSummary)

-- | The broker\'s Amazon Resource Name (ARN).
brokerSummary_brokerArn :: Lens.Lens' BrokerSummary (Prelude.Maybe Prelude.Text)
brokerSummary_brokerArn = Lens.lens (\BrokerSummary' {brokerArn} -> brokerArn) (\s@BrokerSummary' {} a -> s {brokerArn = a} :: BrokerSummary)

-- | The broker\'s deployment mode.
brokerSummary_deploymentMode :: Lens.Lens' BrokerSummary DeploymentMode
brokerSummary_deploymentMode = Lens.lens (\BrokerSummary' {deploymentMode} -> deploymentMode) (\s@BrokerSummary' {} a -> s {deploymentMode = a} :: BrokerSummary)

-- | The type of broker engine.
brokerSummary_engineType :: Lens.Lens' BrokerSummary EngineType
brokerSummary_engineType = Lens.lens (\BrokerSummary' {engineType} -> engineType) (\s@BrokerSummary' {} a -> s {engineType = a} :: BrokerSummary)

instance Data.FromJSON BrokerSummary where
  parseJSON =
    Data.withObject
      "BrokerSummary"
      ( \x ->
          BrokerSummary'
            Prelude.<$> (x Data..:? "brokerName")
            Prelude.<*> (x Data..:? "created")
            Prelude.<*> (x Data..:? "brokerState")
            Prelude.<*> (x Data..:? "brokerId")
            Prelude.<*> (x Data..:? "hostInstanceType")
            Prelude.<*> (x Data..:? "brokerArn")
            Prelude.<*> (x Data..: "deploymentMode")
            Prelude.<*> (x Data..: "engineType")
      )

instance Prelude.Hashable BrokerSummary where
  hashWithSalt _salt BrokerSummary' {..} =
    _salt `Prelude.hashWithSalt` brokerName
      `Prelude.hashWithSalt` created
      `Prelude.hashWithSalt` brokerState
      `Prelude.hashWithSalt` brokerId
      `Prelude.hashWithSalt` hostInstanceType
      `Prelude.hashWithSalt` brokerArn
      `Prelude.hashWithSalt` deploymentMode
      `Prelude.hashWithSalt` engineType

instance Prelude.NFData BrokerSummary where
  rnf BrokerSummary' {..} =
    Prelude.rnf brokerName
      `Prelude.seq` Prelude.rnf created
      `Prelude.seq` Prelude.rnf brokerState
      `Prelude.seq` Prelude.rnf brokerId
      `Prelude.seq` Prelude.rnf hostInstanceType
      `Prelude.seq` Prelude.rnf brokerArn
      `Prelude.seq` Prelude.rnf deploymentMode
      `Prelude.seq` Prelude.rnf engineType
