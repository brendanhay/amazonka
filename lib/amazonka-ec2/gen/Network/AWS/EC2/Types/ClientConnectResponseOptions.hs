{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientConnectResponseOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ClientConnectResponseOptions
  ( ClientConnectResponseOptions (..)
  -- * Smart constructor
  , mkClientConnectResponseOptions
  -- * Lenses
  , ccroEnabled
  , ccroLambdaFunctionArn
  , ccroStatus
  ) where

import qualified Network.AWS.EC2.Types.ClientVpnEndpointAttributeStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The options for managing connection authorization for new client connections.
--
-- /See:/ 'mkClientConnectResponseOptions' smart constructor.
data ClientConnectResponseOptions = ClientConnectResponseOptions'
  { enabled :: Core.Maybe Core.Bool
    -- ^ Indicates whether client connect options are enabled.
  , lambdaFunctionArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the AWS Lambda function used for connection authorization.
  , status :: Core.Maybe Types.ClientVpnEndpointAttributeStatus
    -- ^ The status of any updates to the client connect options.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClientConnectResponseOptions' value with any optional fields omitted.
mkClientConnectResponseOptions
    :: ClientConnectResponseOptions
mkClientConnectResponseOptions
  = ClientConnectResponseOptions'{enabled = Core.Nothing,
                                  lambdaFunctionArn = Core.Nothing, status = Core.Nothing}

-- | Indicates whether client connect options are enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccroEnabled :: Lens.Lens' ClientConnectResponseOptions (Core.Maybe Core.Bool)
ccroEnabled = Lens.field @"enabled"
{-# INLINEABLE ccroEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | The Amazon Resource Name (ARN) of the AWS Lambda function used for connection authorization.
--
-- /Note:/ Consider using 'lambdaFunctionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccroLambdaFunctionArn :: Lens.Lens' ClientConnectResponseOptions (Core.Maybe Core.Text)
ccroLambdaFunctionArn = Lens.field @"lambdaFunctionArn"
{-# INLINEABLE ccroLambdaFunctionArn #-}
{-# DEPRECATED lambdaFunctionArn "Use generic-lens or generic-optics with 'lambdaFunctionArn' instead"  #-}

-- | The status of any updates to the client connect options.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccroStatus :: Lens.Lens' ClientConnectResponseOptions (Core.Maybe Types.ClientVpnEndpointAttributeStatus)
ccroStatus = Lens.field @"status"
{-# INLINEABLE ccroStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromXML ClientConnectResponseOptions where
        parseXML x
          = ClientConnectResponseOptions' Core.<$>
              (x Core..@? "enabled") Core.<*> x Core..@? "lambdaFunctionArn"
                Core.<*> x Core..@? "status"
