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
-- Module      : Network.AWS.MQ.Types.BrokerSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.BrokerSummary where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types.BrokerState
import Network.AWS.MQ.Types.DeploymentMode
import Network.AWS.MQ.Types.EngineType
import qualified Network.AWS.Prelude as Prelude

-- | The Amazon Resource Name (ARN) of the broker.
--
-- /See:/ 'newBrokerSummary' smart constructor.
data BrokerSummary = BrokerSummary'
  { -- | The name of the broker. This value must be unique in your AWS account,
    -- 1-50 characters long, must contain only letters, numbers, dashes, and
    -- underscores, and must not contain whitespaces, brackets, wildcard
    -- characters, or special characters.
    brokerName :: Prelude.Maybe Prelude.Text,
    -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Prelude.Maybe Prelude.Text,
    -- | Required. The type of broker engine.
    engineType :: Prelude.Maybe EngineType,
    -- | The status of the broker.
    brokerState :: Prelude.Maybe BrokerState,
    -- | The broker\'s instance type.
    hostInstanceType :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the broker.
    brokerArn :: Prelude.Maybe Prelude.Text,
    -- | The time when the broker was created.
    created :: Prelude.Maybe Prelude.POSIX,
    -- | Required. The deployment mode of the broker.
    deploymentMode :: Prelude.Maybe DeploymentMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { brokerName = Prelude.Nothing,
      brokerId = Prelude.Nothing,
      engineType = Prelude.Nothing,
      brokerState = Prelude.Nothing,
      hostInstanceType = Prelude.Nothing,
      brokerArn = Prelude.Nothing,
      created = Prelude.Nothing,
      deploymentMode = Prelude.Nothing
    }

-- | The name of the broker. This value must be unique in your AWS account,
-- 1-50 characters long, must contain only letters, numbers, dashes, and
-- underscores, and must not contain whitespaces, brackets, wildcard
-- characters, or special characters.
brokerSummary_brokerName :: Lens.Lens' BrokerSummary (Prelude.Maybe Prelude.Text)
brokerSummary_brokerName = Lens.lens (\BrokerSummary' {brokerName} -> brokerName) (\s@BrokerSummary' {} a -> s {brokerName = a} :: BrokerSummary)

-- | The unique ID that Amazon MQ generates for the broker.
brokerSummary_brokerId :: Lens.Lens' BrokerSummary (Prelude.Maybe Prelude.Text)
brokerSummary_brokerId = Lens.lens (\BrokerSummary' {brokerId} -> brokerId) (\s@BrokerSummary' {} a -> s {brokerId = a} :: BrokerSummary)

-- | Required. The type of broker engine.
brokerSummary_engineType :: Lens.Lens' BrokerSummary (Prelude.Maybe EngineType)
brokerSummary_engineType = Lens.lens (\BrokerSummary' {engineType} -> engineType) (\s@BrokerSummary' {} a -> s {engineType = a} :: BrokerSummary)

-- | The status of the broker.
brokerSummary_brokerState :: Lens.Lens' BrokerSummary (Prelude.Maybe BrokerState)
brokerSummary_brokerState = Lens.lens (\BrokerSummary' {brokerState} -> brokerState) (\s@BrokerSummary' {} a -> s {brokerState = a} :: BrokerSummary)

-- | The broker\'s instance type.
brokerSummary_hostInstanceType :: Lens.Lens' BrokerSummary (Prelude.Maybe Prelude.Text)
brokerSummary_hostInstanceType = Lens.lens (\BrokerSummary' {hostInstanceType} -> hostInstanceType) (\s@BrokerSummary' {} a -> s {hostInstanceType = a} :: BrokerSummary)

-- | The Amazon Resource Name (ARN) of the broker.
brokerSummary_brokerArn :: Lens.Lens' BrokerSummary (Prelude.Maybe Prelude.Text)
brokerSummary_brokerArn = Lens.lens (\BrokerSummary' {brokerArn} -> brokerArn) (\s@BrokerSummary' {} a -> s {brokerArn = a} :: BrokerSummary)

-- | The time when the broker was created.
brokerSummary_created :: Lens.Lens' BrokerSummary (Prelude.Maybe Prelude.UTCTime)
brokerSummary_created = Lens.lens (\BrokerSummary' {created} -> created) (\s@BrokerSummary' {} a -> s {created = a} :: BrokerSummary) Prelude.. Lens.mapping Prelude._Time

-- | Required. The deployment mode of the broker.
brokerSummary_deploymentMode :: Lens.Lens' BrokerSummary (Prelude.Maybe DeploymentMode)
brokerSummary_deploymentMode = Lens.lens (\BrokerSummary' {deploymentMode} -> deploymentMode) (\s@BrokerSummary' {} a -> s {deploymentMode = a} :: BrokerSummary)

instance Prelude.FromJSON BrokerSummary where
  parseJSON =
    Prelude.withObject
      "BrokerSummary"
      ( \x ->
          BrokerSummary'
            Prelude.<$> (x Prelude..:? "brokerName")
            Prelude.<*> (x Prelude..:? "brokerId")
            Prelude.<*> (x Prelude..:? "engineType")
            Prelude.<*> (x Prelude..:? "brokerState")
            Prelude.<*> (x Prelude..:? "hostInstanceType")
            Prelude.<*> (x Prelude..:? "brokerArn")
            Prelude.<*> (x Prelude..:? "created")
            Prelude.<*> (x Prelude..:? "deploymentMode")
      )

instance Prelude.Hashable BrokerSummary

instance Prelude.NFData BrokerSummary
