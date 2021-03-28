{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.EndpointConfigSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.EndpointConfigSummary
  ( EndpointConfigSummary (..)
  -- * Smart constructor
  , mkEndpointConfigSummary
  -- * Lenses
  , ecsEndpointConfigName
  , ecsEndpointConfigArn
  , ecsCreationTime
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.EndpointConfigArn as Types
import qualified Network.AWS.SageMaker.Types.EndpointConfigName as Types

-- | Provides summary information for an endpoint configuration.
--
-- /See:/ 'mkEndpointConfigSummary' smart constructor.
data EndpointConfigSummary = EndpointConfigSummary'
  { endpointConfigName :: Types.EndpointConfigName
    -- ^ The name of the endpoint configuration.
  , endpointConfigArn :: Types.EndpointConfigArn
    -- ^ The Amazon Resource Name (ARN) of the endpoint configuration.
  , creationTime :: Core.NominalDiffTime
    -- ^ A timestamp that shows when the endpoint configuration was created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'EndpointConfigSummary' value with any optional fields omitted.
mkEndpointConfigSummary
    :: Types.EndpointConfigName -- ^ 'endpointConfigName'
    -> Types.EndpointConfigArn -- ^ 'endpointConfigArn'
    -> Core.NominalDiffTime -- ^ 'creationTime'
    -> EndpointConfigSummary
mkEndpointConfigSummary endpointConfigName endpointConfigArn
  creationTime
  = EndpointConfigSummary'{endpointConfigName, endpointConfigArn,
                           creationTime}

-- | The name of the endpoint configuration.
--
-- /Note:/ Consider using 'endpointConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecsEndpointConfigName :: Lens.Lens' EndpointConfigSummary Types.EndpointConfigName
ecsEndpointConfigName = Lens.field @"endpointConfigName"
{-# INLINEABLE ecsEndpointConfigName #-}
{-# DEPRECATED endpointConfigName "Use generic-lens or generic-optics with 'endpointConfigName' instead"  #-}

-- | The Amazon Resource Name (ARN) of the endpoint configuration.
--
-- /Note:/ Consider using 'endpointConfigArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecsEndpointConfigArn :: Lens.Lens' EndpointConfigSummary Types.EndpointConfigArn
ecsEndpointConfigArn = Lens.field @"endpointConfigArn"
{-# INLINEABLE ecsEndpointConfigArn #-}
{-# DEPRECATED endpointConfigArn "Use generic-lens or generic-optics with 'endpointConfigArn' instead"  #-}

-- | A timestamp that shows when the endpoint configuration was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecsCreationTime :: Lens.Lens' EndpointConfigSummary Core.NominalDiffTime
ecsCreationTime = Lens.field @"creationTime"
{-# INLINEABLE ecsCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

instance Core.FromJSON EndpointConfigSummary where
        parseJSON
          = Core.withObject "EndpointConfigSummary" Core.$
              \ x ->
                EndpointConfigSummary' Core.<$>
                  (x Core..: "EndpointConfigName") Core.<*>
                    x Core..: "EndpointConfigArn"
                    Core.<*> x Core..: "CreationTime"
