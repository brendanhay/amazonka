{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowLambdaParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowLambdaParameters
  ( MaintenanceWindowLambdaParameters (..),

    -- * Smart constructor
    mkMaintenanceWindowLambdaParameters,

    -- * Lenses
    mwlpPayload,
    mwlpQualifier,
    mwlpClientContext,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The parameters for a LAMBDA task type.
--
-- For information about specifying and updating task parameters, see 'RegisterTaskWithMaintenanceWindow' and 'UpdateMaintenanceWindowTask' .
--
-- /See:/ 'mkMaintenanceWindowLambdaParameters' smart constructor.
data MaintenanceWindowLambdaParameters = MaintenanceWindowLambdaParameters'
  { payload ::
      Lude.Maybe
        ( Lude.Sensitive
            Lude.Base64
        ),
    qualifier ::
      Lude.Maybe Lude.Text,
    clientContext ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MaintenanceWindowLambdaParameters' with the minimum fields required to make a request.
--
-- * 'clientContext' - Pass client-specific information to the Lambda function that you are invoking. You can then process the client information in your Lambda function as you choose through the context variable.
-- * 'payload' - JSON to provide to your Lambda function as input.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
-- * 'qualifier' - (Optional) Specify a Lambda function version or alias name. If you specify a function version, the action uses the qualified function ARN to invoke a specific Lambda function. If you specify an alias name, the action uses the alias ARN to invoke the Lambda function version to which the alias points.
mkMaintenanceWindowLambdaParameters ::
  MaintenanceWindowLambdaParameters
mkMaintenanceWindowLambdaParameters =
  MaintenanceWindowLambdaParameters'
    { payload = Lude.Nothing,
      qualifier = Lude.Nothing,
      clientContext = Lude.Nothing
    }

-- | JSON to provide to your Lambda function as input.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'payload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwlpPayload :: Lens.Lens' MaintenanceWindowLambdaParameters (Lude.Maybe (Lude.Sensitive Lude.Base64))
mwlpPayload = Lens.lens (payload :: MaintenanceWindowLambdaParameters -> Lude.Maybe (Lude.Sensitive Lude.Base64)) (\s a -> s {payload = a} :: MaintenanceWindowLambdaParameters)
{-# DEPRECATED mwlpPayload "Use generic-lens or generic-optics with 'payload' instead." #-}

-- | (Optional) Specify a Lambda function version or alias name. If you specify a function version, the action uses the qualified function ARN to invoke a specific Lambda function. If you specify an alias name, the action uses the alias ARN to invoke the Lambda function version to which the alias points.
--
-- /Note:/ Consider using 'qualifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwlpQualifier :: Lens.Lens' MaintenanceWindowLambdaParameters (Lude.Maybe Lude.Text)
mwlpQualifier = Lens.lens (qualifier :: MaintenanceWindowLambdaParameters -> Lude.Maybe Lude.Text) (\s a -> s {qualifier = a} :: MaintenanceWindowLambdaParameters)
{-# DEPRECATED mwlpQualifier "Use generic-lens or generic-optics with 'qualifier' instead." #-}

-- | Pass client-specific information to the Lambda function that you are invoking. You can then process the client information in your Lambda function as you choose through the context variable.
--
-- /Note:/ Consider using 'clientContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwlpClientContext :: Lens.Lens' MaintenanceWindowLambdaParameters (Lude.Maybe Lude.Text)
mwlpClientContext = Lens.lens (clientContext :: MaintenanceWindowLambdaParameters -> Lude.Maybe Lude.Text) (\s a -> s {clientContext = a} :: MaintenanceWindowLambdaParameters)
{-# DEPRECATED mwlpClientContext "Use generic-lens or generic-optics with 'clientContext' instead." #-}

instance Lude.FromJSON MaintenanceWindowLambdaParameters where
  parseJSON =
    Lude.withObject
      "MaintenanceWindowLambdaParameters"
      ( \x ->
          MaintenanceWindowLambdaParameters'
            Lude.<$> (x Lude..:? "Payload")
            Lude.<*> (x Lude..:? "Qualifier")
            Lude.<*> (x Lude..:? "ClientContext")
      )

instance Lude.ToJSON MaintenanceWindowLambdaParameters where
  toJSON MaintenanceWindowLambdaParameters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Payload" Lude..=) Lude.<$> payload,
            ("Qualifier" Lude..=) Lude.<$> qualifier,
            ("ClientContext" Lude..=) Lude.<$> clientContext
          ]
      )
