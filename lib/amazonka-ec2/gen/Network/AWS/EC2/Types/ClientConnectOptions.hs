{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientConnectOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ClientConnectOptions
  ( ClientConnectOptions (..)
  -- * Smart constructor
  , mkClientConnectOptions
  -- * Lenses
  , ccoEnabled
  , ccoLambdaFunctionArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The options for managing connection authorization for new client connections.
--
-- /See:/ 'mkClientConnectOptions' smart constructor.
data ClientConnectOptions = ClientConnectOptions'
  { enabled :: Core.Maybe Core.Bool
    -- ^ Indicates whether client connect options are enabled. The default is @false@ (not enabled).
  , lambdaFunctionArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the AWS Lambda function used for connection authorization.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClientConnectOptions' value with any optional fields omitted.
mkClientConnectOptions
    :: ClientConnectOptions
mkClientConnectOptions
  = ClientConnectOptions'{enabled = Core.Nothing,
                          lambdaFunctionArn = Core.Nothing}

-- | Indicates whether client connect options are enabled. The default is @false@ (not enabled).
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccoEnabled :: Lens.Lens' ClientConnectOptions (Core.Maybe Core.Bool)
ccoEnabled = Lens.field @"enabled"
{-# INLINEABLE ccoEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | The Amazon Resource Name (ARN) of the AWS Lambda function used for connection authorization.
--
-- /Note:/ Consider using 'lambdaFunctionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccoLambdaFunctionArn :: Lens.Lens' ClientConnectOptions (Core.Maybe Core.Text)
ccoLambdaFunctionArn = Lens.field @"lambdaFunctionArn"
{-# INLINEABLE ccoLambdaFunctionArn #-}
{-# DEPRECATED lambdaFunctionArn "Use generic-lens or generic-optics with 'lambdaFunctionArn' instead"  #-}

instance Core.ToQuery ClientConnectOptions where
        toQuery ClientConnectOptions{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Enabled") enabled
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LambdaFunctionArn")
                lambdaFunctionArn
