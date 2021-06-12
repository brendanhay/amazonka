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
-- Module      : Network.AWS.MQ.Types.BrokerSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.BrokerSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types.BrokerState
import Network.AWS.MQ.Types.DeploymentMode
import Network.AWS.MQ.Types.EngineType

-- | The Amazon Resource Name (ARN) of the broker.
--
-- /See:/ 'newBrokerSummary' smart constructor.
data BrokerSummary = BrokerSummary'
  { -- | The name of the broker. This value must be unique in your AWS account,
    -- 1-50 characters long, must contain only letters, numbers, dashes, and
    -- underscores, and must not contain whitespaces, brackets, wildcard
    -- characters, or special characters.
    brokerName :: Core.Maybe Core.Text,
    -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Core.Maybe Core.Text,
    -- | Required. The type of broker engine.
    engineType :: Core.Maybe EngineType,
    -- | The status of the broker.
    brokerState :: Core.Maybe BrokerState,
    -- | The broker\'s instance type.
    hostInstanceType :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the broker.
    brokerArn :: Core.Maybe Core.Text,
    -- | The time when the broker was created.
    created :: Core.Maybe Core.POSIX,
    -- | Required. The deployment mode of the broker.
    deploymentMode :: Core.Maybe DeploymentMode
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BrokerSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'brokerName', 'brokerSummary_brokerName' - The name of the broker. This value must be unique in your AWS account,
-- 1-50 characters long, must contain only letters, numbers, dashes, and
-- underscores, and must not contain whitespaces, brackets, wildcard
-- characters, or special characters.
--
-- 'brokerId', 'brokerSummary_brokerId' - The unique ID that Amazon MQ generates for the broker.
--
-- 'engineType', 'brokerSummary_engineType' - Required. The type of broker engine.
--
-- 'brokerState', 'brokerSummary_brokerState' - The status of the broker.
--
-- 'hostInstanceType', 'brokerSummary_hostInstanceType' - The broker\'s instance type.
--
-- 'brokerArn', 'brokerSummary_brokerArn' - The Amazon Resource Name (ARN) of the broker.
--
-- 'created', 'brokerSummary_created' - The time when the broker was created.
--
-- 'deploymentMode', 'brokerSummary_deploymentMode' - Required. The deployment mode of the broker.
newBrokerSummary ::
  BrokerSummary
newBrokerSummary =
  BrokerSummary'
    { brokerName = Core.Nothing,
      brokerId = Core.Nothing,
      engineType = Core.Nothing,
      brokerState = Core.Nothing,
      hostInstanceType = Core.Nothing,
      brokerArn = Core.Nothing,
      created = Core.Nothing,
      deploymentMode = Core.Nothing
    }

-- | The name of the broker. This value must be unique in your AWS account,
-- 1-50 characters long, must contain only letters, numbers, dashes, and
-- underscores, and must not contain whitespaces, brackets, wildcard
-- characters, or special characters.
brokerSummary_brokerName :: Lens.Lens' BrokerSummary (Core.Maybe Core.Text)
brokerSummary_brokerName = Lens.lens (\BrokerSummary' {brokerName} -> brokerName) (\s@BrokerSummary' {} a -> s {brokerName = a} :: BrokerSummary)

-- | The unique ID that Amazon MQ generates for the broker.
brokerSummary_brokerId :: Lens.Lens' BrokerSummary (Core.Maybe Core.Text)
brokerSummary_brokerId = Lens.lens (\BrokerSummary' {brokerId} -> brokerId) (\s@BrokerSummary' {} a -> s {brokerId = a} :: BrokerSummary)

-- | Required. The type of broker engine.
brokerSummary_engineType :: Lens.Lens' BrokerSummary (Core.Maybe EngineType)
brokerSummary_engineType = Lens.lens (\BrokerSummary' {engineType} -> engineType) (\s@BrokerSummary' {} a -> s {engineType = a} :: BrokerSummary)

-- | The status of the broker.
brokerSummary_brokerState :: Lens.Lens' BrokerSummary (Core.Maybe BrokerState)
brokerSummary_brokerState = Lens.lens (\BrokerSummary' {brokerState} -> brokerState) (\s@BrokerSummary' {} a -> s {brokerState = a} :: BrokerSummary)

-- | The broker\'s instance type.
brokerSummary_hostInstanceType :: Lens.Lens' BrokerSummary (Core.Maybe Core.Text)
brokerSummary_hostInstanceType = Lens.lens (\BrokerSummary' {hostInstanceType} -> hostInstanceType) (\s@BrokerSummary' {} a -> s {hostInstanceType = a} :: BrokerSummary)

-- | The Amazon Resource Name (ARN) of the broker.
brokerSummary_brokerArn :: Lens.Lens' BrokerSummary (Core.Maybe Core.Text)
brokerSummary_brokerArn = Lens.lens (\BrokerSummary' {brokerArn} -> brokerArn) (\s@BrokerSummary' {} a -> s {brokerArn = a} :: BrokerSummary)

-- | The time when the broker was created.
brokerSummary_created :: Lens.Lens' BrokerSummary (Core.Maybe Core.UTCTime)
brokerSummary_created = Lens.lens (\BrokerSummary' {created} -> created) (\s@BrokerSummary' {} a -> s {created = a} :: BrokerSummary) Core.. Lens.mapping Core._Time

-- | Required. The deployment mode of the broker.
brokerSummary_deploymentMode :: Lens.Lens' BrokerSummary (Core.Maybe DeploymentMode)
brokerSummary_deploymentMode = Lens.lens (\BrokerSummary' {deploymentMode} -> deploymentMode) (\s@BrokerSummary' {} a -> s {deploymentMode = a} :: BrokerSummary)

instance Core.FromJSON BrokerSummary where
  parseJSON =
    Core.withObject
      "BrokerSummary"
      ( \x ->
          BrokerSummary'
            Core.<$> (x Core..:? "brokerName")
            Core.<*> (x Core..:? "brokerId")
            Core.<*> (x Core..:? "engineType")
            Core.<*> (x Core..:? "brokerState")
            Core.<*> (x Core..:? "hostInstanceType")
            Core.<*> (x Core..:? "brokerArn")
            Core.<*> (x Core..:? "created")
            Core.<*> (x Core..:? "deploymentMode")
      )

instance Core.Hashable BrokerSummary

instance Core.NFData BrokerSummary
