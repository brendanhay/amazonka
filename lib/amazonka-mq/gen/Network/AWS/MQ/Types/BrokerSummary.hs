{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.BrokerSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.BrokerSummary
  ( BrokerSummary (..),

    -- * Smart constructor
    mkBrokerSummary,

    -- * Lenses
    bsBrokerName,
    bsBrokerState,
    bsCreated,
    bsDeploymentMode,
    bsBrokerId,
    bsEngineType,
    bsBrokerARN,
    bsHostInstanceType,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types.BrokerState
import Network.AWS.MQ.Types.DeploymentMode
import Network.AWS.MQ.Types.EngineType
import qualified Network.AWS.Prelude as Lude

-- | The Amazon Resource Name (ARN) of the broker.
--
-- /See:/ 'mkBrokerSummary' smart constructor.
data BrokerSummary = BrokerSummary'
  { brokerName ::
      Lude.Maybe Lude.Text,
    brokerState :: Lude.Maybe BrokerState,
    created :: Lude.Maybe Lude.Timestamp,
    deploymentMode :: Lude.Maybe DeploymentMode,
    brokerId :: Lude.Maybe Lude.Text,
    engineType :: Lude.Maybe EngineType,
    brokerARN :: Lude.Maybe Lude.Text,
    hostInstanceType :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BrokerSummary' with the minimum fields required to make a request.
--
-- * 'brokerARN' - The Amazon Resource Name (ARN) of the broker.
-- * 'brokerId' - The unique ID that Amazon MQ generates for the broker.
-- * 'brokerName' - The name of the broker. This value must be unique in your AWS account, 1-50 characters long, must contain only letters, numbers, dashes, and underscores, and must not contain whitespaces, brackets, wildcard characters, or special characters.
-- * 'brokerState' - The status of the broker.
-- * 'created' - The time when the broker was created.
-- * 'deploymentMode' - Required. The deployment mode of the broker.
-- * 'engineType' - Required. The type of broker engine.
-- * 'hostInstanceType' - The broker's instance type.
mkBrokerSummary ::
  BrokerSummary
mkBrokerSummary =
  BrokerSummary'
    { brokerName = Lude.Nothing,
      brokerState = Lude.Nothing,
      created = Lude.Nothing,
      deploymentMode = Lude.Nothing,
      brokerId = Lude.Nothing,
      engineType = Lude.Nothing,
      brokerARN = Lude.Nothing,
      hostInstanceType = Lude.Nothing
    }

-- | The name of the broker. This value must be unique in your AWS account, 1-50 characters long, must contain only letters, numbers, dashes, and underscores, and must not contain whitespaces, brackets, wildcard characters, or special characters.
--
-- /Note:/ Consider using 'brokerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsBrokerName :: Lens.Lens' BrokerSummary (Lude.Maybe Lude.Text)
bsBrokerName = Lens.lens (brokerName :: BrokerSummary -> Lude.Maybe Lude.Text) (\s a -> s {brokerName = a} :: BrokerSummary)
{-# DEPRECATED bsBrokerName "Use generic-lens or generic-optics with 'brokerName' instead." #-}

-- | The status of the broker.
--
-- /Note:/ Consider using 'brokerState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsBrokerState :: Lens.Lens' BrokerSummary (Lude.Maybe BrokerState)
bsBrokerState = Lens.lens (brokerState :: BrokerSummary -> Lude.Maybe BrokerState) (\s a -> s {brokerState = a} :: BrokerSummary)
{-# DEPRECATED bsBrokerState "Use generic-lens or generic-optics with 'brokerState' instead." #-}

-- | The time when the broker was created.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsCreated :: Lens.Lens' BrokerSummary (Lude.Maybe Lude.Timestamp)
bsCreated = Lens.lens (created :: BrokerSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {created = a} :: BrokerSummary)
{-# DEPRECATED bsCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | Required. The deployment mode of the broker.
--
-- /Note:/ Consider using 'deploymentMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsDeploymentMode :: Lens.Lens' BrokerSummary (Lude.Maybe DeploymentMode)
bsDeploymentMode = Lens.lens (deploymentMode :: BrokerSummary -> Lude.Maybe DeploymentMode) (\s a -> s {deploymentMode = a} :: BrokerSummary)
{-# DEPRECATED bsDeploymentMode "Use generic-lens or generic-optics with 'deploymentMode' instead." #-}

-- | The unique ID that Amazon MQ generates for the broker.
--
-- /Note:/ Consider using 'brokerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsBrokerId :: Lens.Lens' BrokerSummary (Lude.Maybe Lude.Text)
bsBrokerId = Lens.lens (brokerId :: BrokerSummary -> Lude.Maybe Lude.Text) (\s a -> s {brokerId = a} :: BrokerSummary)
{-# DEPRECATED bsBrokerId "Use generic-lens or generic-optics with 'brokerId' instead." #-}

-- | Required. The type of broker engine.
--
-- /Note:/ Consider using 'engineType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsEngineType :: Lens.Lens' BrokerSummary (Lude.Maybe EngineType)
bsEngineType = Lens.lens (engineType :: BrokerSummary -> Lude.Maybe EngineType) (\s a -> s {engineType = a} :: BrokerSummary)
{-# DEPRECATED bsEngineType "Use generic-lens or generic-optics with 'engineType' instead." #-}

-- | The Amazon Resource Name (ARN) of the broker.
--
-- /Note:/ Consider using 'brokerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsBrokerARN :: Lens.Lens' BrokerSummary (Lude.Maybe Lude.Text)
bsBrokerARN = Lens.lens (brokerARN :: BrokerSummary -> Lude.Maybe Lude.Text) (\s a -> s {brokerARN = a} :: BrokerSummary)
{-# DEPRECATED bsBrokerARN "Use generic-lens or generic-optics with 'brokerARN' instead." #-}

-- | The broker's instance type.
--
-- /Note:/ Consider using 'hostInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsHostInstanceType :: Lens.Lens' BrokerSummary (Lude.Maybe Lude.Text)
bsHostInstanceType = Lens.lens (hostInstanceType :: BrokerSummary -> Lude.Maybe Lude.Text) (\s a -> s {hostInstanceType = a} :: BrokerSummary)
{-# DEPRECATED bsHostInstanceType "Use generic-lens or generic-optics with 'hostInstanceType' instead." #-}

instance Lude.FromJSON BrokerSummary where
  parseJSON =
    Lude.withObject
      "BrokerSummary"
      ( \x ->
          BrokerSummary'
            Lude.<$> (x Lude..:? "brokerName")
            Lude.<*> (x Lude..:? "brokerState")
            Lude.<*> (x Lude..:? "created")
            Lude.<*> (x Lude..:? "deploymentMode")
            Lude.<*> (x Lude..:? "brokerId")
            Lude.<*> (x Lude..:? "engineType")
            Lude.<*> (x Lude..:? "brokerArn")
            Lude.<*> (x Lude..:? "hostInstanceType")
      )
