{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.BrokerSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MQ.Types.BrokerSummary
  ( BrokerSummary (..)
  -- * Smart constructor
  , mkBrokerSummary
  -- * Lenses
  , bsBrokerArn
  , bsBrokerId
  , bsBrokerName
  , bsBrokerState
  , bsCreated
  , bsDeploymentMode
  , bsEngineType
  , bsHostInstanceType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MQ.Types.BrokerState as Types
import qualified Network.AWS.MQ.Types.DeploymentMode as Types
import qualified Network.AWS.MQ.Types.EngineType as Types
import qualified Network.AWS.Prelude as Core

-- | The Amazon Resource Name (ARN) of the broker.
--
-- /See:/ 'mkBrokerSummary' smart constructor.
data BrokerSummary = BrokerSummary'
  { brokerArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the broker.
  , brokerId :: Core.Maybe Core.Text
    -- ^ The unique ID that Amazon MQ generates for the broker.
  , brokerName :: Core.Maybe Core.Text
    -- ^ The name of the broker. This value must be unique in your AWS account, 1-50 characters long, must contain only letters, numbers, dashes, and underscores, and must not contain whitespaces, brackets, wildcard characters, or special characters.
  , brokerState :: Core.Maybe Types.BrokerState
    -- ^ The status of the broker.
  , created :: Core.Maybe Core.UTCTime
    -- ^ The time when the broker was created.
  , deploymentMode :: Core.Maybe Types.DeploymentMode
    -- ^ Required. The deployment mode of the broker.
  , engineType :: Core.Maybe Types.EngineType
    -- ^ Required. The type of broker engine.
  , hostInstanceType :: Core.Maybe Core.Text
    -- ^ The broker's instance type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'BrokerSummary' value with any optional fields omitted.
mkBrokerSummary
    :: BrokerSummary
mkBrokerSummary
  = BrokerSummary'{brokerArn = Core.Nothing, brokerId = Core.Nothing,
                   brokerName = Core.Nothing, brokerState = Core.Nothing,
                   created = Core.Nothing, deploymentMode = Core.Nothing,
                   engineType = Core.Nothing, hostInstanceType = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the broker.
--
-- /Note:/ Consider using 'brokerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsBrokerArn :: Lens.Lens' BrokerSummary (Core.Maybe Core.Text)
bsBrokerArn = Lens.field @"brokerArn"
{-# INLINEABLE bsBrokerArn #-}
{-# DEPRECATED brokerArn "Use generic-lens or generic-optics with 'brokerArn' instead"  #-}

-- | The unique ID that Amazon MQ generates for the broker.
--
-- /Note:/ Consider using 'brokerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsBrokerId :: Lens.Lens' BrokerSummary (Core.Maybe Core.Text)
bsBrokerId = Lens.field @"brokerId"
{-# INLINEABLE bsBrokerId #-}
{-# DEPRECATED brokerId "Use generic-lens or generic-optics with 'brokerId' instead"  #-}

-- | The name of the broker. This value must be unique in your AWS account, 1-50 characters long, must contain only letters, numbers, dashes, and underscores, and must not contain whitespaces, brackets, wildcard characters, or special characters.
--
-- /Note:/ Consider using 'brokerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsBrokerName :: Lens.Lens' BrokerSummary (Core.Maybe Core.Text)
bsBrokerName = Lens.field @"brokerName"
{-# INLINEABLE bsBrokerName #-}
{-# DEPRECATED brokerName "Use generic-lens or generic-optics with 'brokerName' instead"  #-}

-- | The status of the broker.
--
-- /Note:/ Consider using 'brokerState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsBrokerState :: Lens.Lens' BrokerSummary (Core.Maybe Types.BrokerState)
bsBrokerState = Lens.field @"brokerState"
{-# INLINEABLE bsBrokerState #-}
{-# DEPRECATED brokerState "Use generic-lens or generic-optics with 'brokerState' instead"  #-}

-- | The time when the broker was created.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsCreated :: Lens.Lens' BrokerSummary (Core.Maybe Core.UTCTime)
bsCreated = Lens.field @"created"
{-# INLINEABLE bsCreated #-}
{-# DEPRECATED created "Use generic-lens or generic-optics with 'created' instead"  #-}

-- | Required. The deployment mode of the broker.
--
-- /Note:/ Consider using 'deploymentMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsDeploymentMode :: Lens.Lens' BrokerSummary (Core.Maybe Types.DeploymentMode)
bsDeploymentMode = Lens.field @"deploymentMode"
{-# INLINEABLE bsDeploymentMode #-}
{-# DEPRECATED deploymentMode "Use generic-lens or generic-optics with 'deploymentMode' instead"  #-}

-- | Required. The type of broker engine.
--
-- /Note:/ Consider using 'engineType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsEngineType :: Lens.Lens' BrokerSummary (Core.Maybe Types.EngineType)
bsEngineType = Lens.field @"engineType"
{-# INLINEABLE bsEngineType #-}
{-# DEPRECATED engineType "Use generic-lens or generic-optics with 'engineType' instead"  #-}

-- | The broker's instance type.
--
-- /Note:/ Consider using 'hostInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsHostInstanceType :: Lens.Lens' BrokerSummary (Core.Maybe Core.Text)
bsHostInstanceType = Lens.field @"hostInstanceType"
{-# INLINEABLE bsHostInstanceType #-}
{-# DEPRECATED hostInstanceType "Use generic-lens or generic-optics with 'hostInstanceType' instead"  #-}

instance Core.FromJSON BrokerSummary where
        parseJSON
          = Core.withObject "BrokerSummary" Core.$
              \ x ->
                BrokerSummary' Core.<$>
                  (x Core..:? "brokerArn") Core.<*> x Core..:? "brokerId" Core.<*>
                    x Core..:? "brokerName"
                    Core.<*> x Core..:? "brokerState"
                    Core.<*> x Core..:? "created"
                    Core.<*> x Core..:? "deploymentMode"
                    Core.<*> x Core..:? "engineType"
                    Core.<*> x Core..:? "hostInstanceType"
