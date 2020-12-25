{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.FunctionConfigurationEnvironment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.FunctionConfigurationEnvironment
  ( FunctionConfigurationEnvironment (..),

    -- * Smart constructor
    mkFunctionConfigurationEnvironment,

    -- * Lenses
    fceAccessSysfs,
    fceExecution,
    fceResourceAccessPolicies,
    fceVariables,
  )
where

import qualified Network.AWS.Greengrass.Types.FunctionExecutionConfig as Types
import qualified Network.AWS.Greengrass.Types.ResourceAccessPolicy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The environment configuration of the function.
--
-- /See:/ 'mkFunctionConfigurationEnvironment' smart constructor.
data FunctionConfigurationEnvironment = FunctionConfigurationEnvironment'
  { -- | If true, the Lambda function is allowed to access the host's /sys folder. Use this when the Lambda function needs to read device information from /sys. This setting applies only when you run the Lambda function in a Greengrass container.
    accessSysfs :: Core.Maybe Core.Bool,
    -- | Configuration related to executing the Lambda function
    execution :: Core.Maybe Types.FunctionExecutionConfig,
    -- | A list of the resources, with their permissions, to which the Lambda function will be granted access. A Lambda function can have at most 10 resources. ResourceAccessPolicies apply only when you run the Lambda function in a Greengrass container.
    resourceAccessPolicies :: Core.Maybe [Types.ResourceAccessPolicy],
    -- | Environment variables for the Lambda function's configuration.
    variables :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FunctionConfigurationEnvironment' value with any optional fields omitted.
mkFunctionConfigurationEnvironment ::
  FunctionConfigurationEnvironment
mkFunctionConfigurationEnvironment =
  FunctionConfigurationEnvironment'
    { accessSysfs = Core.Nothing,
      execution = Core.Nothing,
      resourceAccessPolicies = Core.Nothing,
      variables = Core.Nothing
    }

-- | If true, the Lambda function is allowed to access the host's /sys folder. Use this when the Lambda function needs to read device information from /sys. This setting applies only when you run the Lambda function in a Greengrass container.
--
-- /Note:/ Consider using 'accessSysfs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fceAccessSysfs :: Lens.Lens' FunctionConfigurationEnvironment (Core.Maybe Core.Bool)
fceAccessSysfs = Lens.field @"accessSysfs"
{-# DEPRECATED fceAccessSysfs "Use generic-lens or generic-optics with 'accessSysfs' instead." #-}

-- | Configuration related to executing the Lambda function
--
-- /Note:/ Consider using 'execution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fceExecution :: Lens.Lens' FunctionConfigurationEnvironment (Core.Maybe Types.FunctionExecutionConfig)
fceExecution = Lens.field @"execution"
{-# DEPRECATED fceExecution "Use generic-lens or generic-optics with 'execution' instead." #-}

-- | A list of the resources, with their permissions, to which the Lambda function will be granted access. A Lambda function can have at most 10 resources. ResourceAccessPolicies apply only when you run the Lambda function in a Greengrass container.
--
-- /Note:/ Consider using 'resourceAccessPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fceResourceAccessPolicies :: Lens.Lens' FunctionConfigurationEnvironment (Core.Maybe [Types.ResourceAccessPolicy])
fceResourceAccessPolicies = Lens.field @"resourceAccessPolicies"
{-# DEPRECATED fceResourceAccessPolicies "Use generic-lens or generic-optics with 'resourceAccessPolicies' instead." #-}

-- | Environment variables for the Lambda function's configuration.
--
-- /Note:/ Consider using 'variables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fceVariables :: Lens.Lens' FunctionConfigurationEnvironment (Core.Maybe (Core.HashMap Core.Text Core.Text))
fceVariables = Lens.field @"variables"
{-# DEPRECATED fceVariables "Use generic-lens or generic-optics with 'variables' instead." #-}

instance Core.FromJSON FunctionConfigurationEnvironment where
  toJSON FunctionConfigurationEnvironment {..} =
    Core.object
      ( Core.catMaybes
          [ ("AccessSysfs" Core..=) Core.<$> accessSysfs,
            ("Execution" Core..=) Core.<$> execution,
            ("ResourceAccessPolicies" Core..=) Core.<$> resourceAccessPolicies,
            ("Variables" Core..=) Core.<$> variables
          ]
      )

instance Core.FromJSON FunctionConfigurationEnvironment where
  parseJSON =
    Core.withObject "FunctionConfigurationEnvironment" Core.$
      \x ->
        FunctionConfigurationEnvironment'
          Core.<$> (x Core..:? "AccessSysfs")
          Core.<*> (x Core..:? "Execution")
          Core.<*> (x Core..:? "ResourceAccessPolicies")
          Core.<*> (x Core..:? "Variables")
