{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientConnectOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientConnectOptions
  ( ClientConnectOptions (..),

    -- * Smart constructor
    mkClientConnectOptions,

    -- * Lenses
    ccoEnabled,
    ccoLambdaFunctionArn,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The options for managing connection authorization for new client connections.
--
-- /See:/ 'mkClientConnectOptions' smart constructor.
data ClientConnectOptions = ClientConnectOptions'
  { -- | Indicates whether client connect options are enabled. The default is @false@ (not enabled).
    enabled :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) of the AWS Lambda function used for connection authorization.
    lambdaFunctionArn :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClientConnectOptions' value with any optional fields omitted.
mkClientConnectOptions ::
  ClientConnectOptions
mkClientConnectOptions =
  ClientConnectOptions'
    { enabled = Core.Nothing,
      lambdaFunctionArn = Core.Nothing
    }

-- | Indicates whether client connect options are enabled. The default is @false@ (not enabled).
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccoEnabled :: Lens.Lens' ClientConnectOptions (Core.Maybe Core.Bool)
ccoEnabled = Lens.field @"enabled"
{-# DEPRECATED ccoEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Lambda function used for connection authorization.
--
-- /Note:/ Consider using 'lambdaFunctionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccoLambdaFunctionArn :: Lens.Lens' ClientConnectOptions (Core.Maybe Types.String)
ccoLambdaFunctionArn = Lens.field @"lambdaFunctionArn"
{-# DEPRECATED ccoLambdaFunctionArn "Use generic-lens or generic-optics with 'lambdaFunctionArn' instead." #-}
