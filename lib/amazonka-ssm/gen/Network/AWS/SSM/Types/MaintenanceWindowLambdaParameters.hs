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
    mwlpClientContext,
    mwlpPayload,
    mwlpQualifier,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.ClientContext as Types
import qualified Network.AWS.SSM.Types.Qualifier as Types

-- | The parameters for a LAMBDA task type.
--
-- For information about specifying and updating task parameters, see 'RegisterTaskWithMaintenanceWindow' and 'UpdateMaintenanceWindowTask' .
--
-- /See:/ 'mkMaintenanceWindowLambdaParameters' smart constructor.
data MaintenanceWindowLambdaParameters = MaintenanceWindowLambdaParameters'
  { -- | Pass client-specific information to the Lambda function that you are invoking. You can then process the client information in your Lambda function as you choose through the context variable.
    clientContext :: Core.Maybe Types.ClientContext,
    -- | JSON to provide to your Lambda function as input.
    payload :: Core.Maybe (Core.Sensitive Core.Base64),
    -- | (Optional) Specify a Lambda function version or alias name. If you specify a function version, the action uses the qualified function ARN to invoke a specific Lambda function. If you specify an alias name, the action uses the alias ARN to invoke the Lambda function version to which the alias points.
    qualifier :: Core.Maybe Types.Qualifier
  }
  deriving stock (Core.Eq, Core.Ord, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MaintenanceWindowLambdaParameters' value with any optional fields omitted.
mkMaintenanceWindowLambdaParameters ::
  MaintenanceWindowLambdaParameters
mkMaintenanceWindowLambdaParameters =
  MaintenanceWindowLambdaParameters'
    { clientContext = Core.Nothing,
      payload = Core.Nothing,
      qualifier = Core.Nothing
    }

-- | Pass client-specific information to the Lambda function that you are invoking. You can then process the client information in your Lambda function as you choose through the context variable.
--
-- /Note:/ Consider using 'clientContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwlpClientContext :: Lens.Lens' MaintenanceWindowLambdaParameters (Core.Maybe Types.ClientContext)
mwlpClientContext = Lens.field @"clientContext"
{-# DEPRECATED mwlpClientContext "Use generic-lens or generic-optics with 'clientContext' instead." #-}

-- | JSON to provide to your Lambda function as input.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'payload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwlpPayload :: Lens.Lens' MaintenanceWindowLambdaParameters (Core.Maybe (Core.Sensitive Core.Base64))
mwlpPayload = Lens.field @"payload"
{-# DEPRECATED mwlpPayload "Use generic-lens or generic-optics with 'payload' instead." #-}

-- | (Optional) Specify a Lambda function version or alias name. If you specify a function version, the action uses the qualified function ARN to invoke a specific Lambda function. If you specify an alias name, the action uses the alias ARN to invoke the Lambda function version to which the alias points.
--
-- /Note:/ Consider using 'qualifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwlpQualifier :: Lens.Lens' MaintenanceWindowLambdaParameters (Core.Maybe Types.Qualifier)
mwlpQualifier = Lens.field @"qualifier"
{-# DEPRECATED mwlpQualifier "Use generic-lens or generic-optics with 'qualifier' instead." #-}

instance Core.FromJSON MaintenanceWindowLambdaParameters where
  toJSON MaintenanceWindowLambdaParameters {..} =
    Core.object
      ( Core.catMaybes
          [ ("ClientContext" Core..=) Core.<$> clientContext,
            ("Payload" Core..=) Core.<$> payload,
            ("Qualifier" Core..=) Core.<$> qualifier
          ]
      )

instance Core.FromJSON MaintenanceWindowLambdaParameters where
  parseJSON =
    Core.withObject "MaintenanceWindowLambdaParameters" Core.$
      \x ->
        MaintenanceWindowLambdaParameters'
          Core.<$> (x Core..:? "ClientContext")
          Core.<*> (x Core..:? "Payload")
          Core.<*> (x Core..:? "Qualifier")
