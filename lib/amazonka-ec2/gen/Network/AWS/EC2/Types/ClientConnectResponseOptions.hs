{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientConnectResponseOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientConnectResponseOptions
  ( ClientConnectResponseOptions (..),

    -- * Smart constructor
    mkClientConnectResponseOptions,

    -- * Lenses
    ccroEnabled,
    ccroLambdaFunctionArn,
    ccroStatus,
  )
where

import qualified Network.AWS.EC2.Types.ClientVpnEndpointAttributeStatus as Types
import qualified Network.AWS.EC2.Types.LambdaFunctionArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The options for managing connection authorization for new client connections.
--
-- /See:/ 'mkClientConnectResponseOptions' smart constructor.
data ClientConnectResponseOptions = ClientConnectResponseOptions'
  { -- | Indicates whether client connect options are enabled.
    enabled :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) of the AWS Lambda function used for connection authorization.
    lambdaFunctionArn :: Core.Maybe Types.LambdaFunctionArn,
    -- | The status of any updates to the client connect options.
    status :: Core.Maybe Types.ClientVpnEndpointAttributeStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClientConnectResponseOptions' value with any optional fields omitted.
mkClientConnectResponseOptions ::
  ClientConnectResponseOptions
mkClientConnectResponseOptions =
  ClientConnectResponseOptions'
    { enabled = Core.Nothing,
      lambdaFunctionArn = Core.Nothing,
      status = Core.Nothing
    }

-- | Indicates whether client connect options are enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccroEnabled :: Lens.Lens' ClientConnectResponseOptions (Core.Maybe Core.Bool)
ccroEnabled = Lens.field @"enabled"
{-# DEPRECATED ccroEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Lambda function used for connection authorization.
--
-- /Note:/ Consider using 'lambdaFunctionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccroLambdaFunctionArn :: Lens.Lens' ClientConnectResponseOptions (Core.Maybe Types.LambdaFunctionArn)
ccroLambdaFunctionArn = Lens.field @"lambdaFunctionArn"
{-# DEPRECATED ccroLambdaFunctionArn "Use generic-lens or generic-optics with 'lambdaFunctionArn' instead." #-}

-- | The status of any updates to the client connect options.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccroStatus :: Lens.Lens' ClientConnectResponseOptions (Core.Maybe Types.ClientVpnEndpointAttributeStatus)
ccroStatus = Lens.field @"status"
{-# DEPRECATED ccroStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromXML ClientConnectResponseOptions where
  parseXML x =
    ClientConnectResponseOptions'
      Core.<$> (x Core..@? "enabled")
      Core.<*> (x Core..@? "lambdaFunctionArn")
      Core.<*> (x Core..@? "status")
