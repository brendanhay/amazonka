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
    ccoLambdaFunctionARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The options for managing connection authorization for new client connections.
--
-- /See:/ 'mkClientConnectOptions' smart constructor.
data ClientConnectOptions = ClientConnectOptions'
  { enabled ::
      Lude.Maybe Lude.Bool,
    lambdaFunctionARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClientConnectOptions' with the minimum fields required to make a request.
--
-- * 'enabled' - Indicates whether client connect options are enabled. The default is @false@ (not enabled).
-- * 'lambdaFunctionARN' - The Amazon Resource Name (ARN) of the AWS Lambda function used for connection authorization.
mkClientConnectOptions ::
  ClientConnectOptions
mkClientConnectOptions =
  ClientConnectOptions'
    { enabled = Lude.Nothing,
      lambdaFunctionARN = Lude.Nothing
    }

-- | Indicates whether client connect options are enabled. The default is @false@ (not enabled).
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccoEnabled :: Lens.Lens' ClientConnectOptions (Lude.Maybe Lude.Bool)
ccoEnabled = Lens.lens (enabled :: ClientConnectOptions -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: ClientConnectOptions)
{-# DEPRECATED ccoEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Lambda function used for connection authorization.
--
-- /Note:/ Consider using 'lambdaFunctionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccoLambdaFunctionARN :: Lens.Lens' ClientConnectOptions (Lude.Maybe Lude.Text)
ccoLambdaFunctionARN = Lens.lens (lambdaFunctionARN :: ClientConnectOptions -> Lude.Maybe Lude.Text) (\s a -> s {lambdaFunctionARN = a} :: ClientConnectOptions)
{-# DEPRECATED ccoLambdaFunctionARN "Use generic-lens or generic-optics with 'lambdaFunctionARN' instead." #-}

instance Lude.ToQuery ClientConnectOptions where
  toQuery ClientConnectOptions' {..} =
    Lude.mconcat
      [ "Enabled" Lude.=: enabled,
        "LambdaFunctionArn" Lude.=: lambdaFunctionARN
      ]
