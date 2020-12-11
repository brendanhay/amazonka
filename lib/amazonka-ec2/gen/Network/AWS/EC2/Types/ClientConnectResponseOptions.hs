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
    ccroStatus,
    ccroEnabled,
    ccroLambdaFunctionARN,
  )
where

import Network.AWS.EC2.Types.ClientVPNEndpointAttributeStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The options for managing connection authorization for new client connections.
--
-- /See:/ 'mkClientConnectResponseOptions' smart constructor.
data ClientConnectResponseOptions = ClientConnectResponseOptions'
  { status ::
      Lude.Maybe
        ClientVPNEndpointAttributeStatus,
    enabled :: Lude.Maybe Lude.Bool,
    lambdaFunctionARN ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClientConnectResponseOptions' with the minimum fields required to make a request.
--
-- * 'enabled' - Indicates whether client connect options are enabled.
-- * 'lambdaFunctionARN' - The Amazon Resource Name (ARN) of the AWS Lambda function used for connection authorization.
-- * 'status' - The status of any updates to the client connect options.
mkClientConnectResponseOptions ::
  ClientConnectResponseOptions
mkClientConnectResponseOptions =
  ClientConnectResponseOptions'
    { status = Lude.Nothing,
      enabled = Lude.Nothing,
      lambdaFunctionARN = Lude.Nothing
    }

-- | The status of any updates to the client connect options.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccroStatus :: Lens.Lens' ClientConnectResponseOptions (Lude.Maybe ClientVPNEndpointAttributeStatus)
ccroStatus = Lens.lens (status :: ClientConnectResponseOptions -> Lude.Maybe ClientVPNEndpointAttributeStatus) (\s a -> s {status = a} :: ClientConnectResponseOptions)
{-# DEPRECATED ccroStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Indicates whether client connect options are enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccroEnabled :: Lens.Lens' ClientConnectResponseOptions (Lude.Maybe Lude.Bool)
ccroEnabled = Lens.lens (enabled :: ClientConnectResponseOptions -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: ClientConnectResponseOptions)
{-# DEPRECATED ccroEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Lambda function used for connection authorization.
--
-- /Note:/ Consider using 'lambdaFunctionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccroLambdaFunctionARN :: Lens.Lens' ClientConnectResponseOptions (Lude.Maybe Lude.Text)
ccroLambdaFunctionARN = Lens.lens (lambdaFunctionARN :: ClientConnectResponseOptions -> Lude.Maybe Lude.Text) (\s a -> s {lambdaFunctionARN = a} :: ClientConnectResponseOptions)
{-# DEPRECATED ccroLambdaFunctionARN "Use generic-lens or generic-optics with 'lambdaFunctionARN' instead." #-}

instance Lude.FromXML ClientConnectResponseOptions where
  parseXML x =
    ClientConnectResponseOptions'
      Lude.<$> (x Lude..@? "status")
      Lude.<*> (x Lude..@? "enabled")
      Lude.<*> (x Lude..@? "lambdaFunctionArn")
