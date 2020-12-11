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
    fceVariables,
    fceExecution,
    fceResourceAccessPolicies,
    fceAccessSysfs,
  )
where

import Network.AWS.Greengrass.Types.FunctionExecutionConfig
import Network.AWS.Greengrass.Types.ResourceAccessPolicy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The environment configuration of the function.
--
-- /See:/ 'mkFunctionConfigurationEnvironment' smart constructor.
data FunctionConfigurationEnvironment = FunctionConfigurationEnvironment'
  { variables ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (Lude.Text)
        ),
    execution ::
      Lude.Maybe
        FunctionExecutionConfig,
    resourceAccessPolicies ::
      Lude.Maybe
        [ResourceAccessPolicy],
    accessSysfs ::
      Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FunctionConfigurationEnvironment' with the minimum fields required to make a request.
--
-- * 'accessSysfs' - If true, the Lambda function is allowed to access the host's /sys folder. Use this when the Lambda function needs to read device information from /sys. This setting applies only when you run the Lambda function in a Greengrass container.
-- * 'execution' - Configuration related to executing the Lambda function
-- * 'resourceAccessPolicies' - A list of the resources, with their permissions, to which the Lambda function will be granted access. A Lambda function can have at most 10 resources. ResourceAccessPolicies apply only when you run the Lambda function in a Greengrass container.
-- * 'variables' - Environment variables for the Lambda function's configuration.
mkFunctionConfigurationEnvironment ::
  FunctionConfigurationEnvironment
mkFunctionConfigurationEnvironment =
  FunctionConfigurationEnvironment'
    { variables = Lude.Nothing,
      execution = Lude.Nothing,
      resourceAccessPolicies = Lude.Nothing,
      accessSysfs = Lude.Nothing
    }

-- | Environment variables for the Lambda function's configuration.
--
-- /Note:/ Consider using 'variables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fceVariables :: Lens.Lens' FunctionConfigurationEnvironment (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
fceVariables = Lens.lens (variables :: FunctionConfigurationEnvironment -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {variables = a} :: FunctionConfigurationEnvironment)
{-# DEPRECATED fceVariables "Use generic-lens or generic-optics with 'variables' instead." #-}

-- | Configuration related to executing the Lambda function
--
-- /Note:/ Consider using 'execution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fceExecution :: Lens.Lens' FunctionConfigurationEnvironment (Lude.Maybe FunctionExecutionConfig)
fceExecution = Lens.lens (execution :: FunctionConfigurationEnvironment -> Lude.Maybe FunctionExecutionConfig) (\s a -> s {execution = a} :: FunctionConfigurationEnvironment)
{-# DEPRECATED fceExecution "Use generic-lens or generic-optics with 'execution' instead." #-}

-- | A list of the resources, with their permissions, to which the Lambda function will be granted access. A Lambda function can have at most 10 resources. ResourceAccessPolicies apply only when you run the Lambda function in a Greengrass container.
--
-- /Note:/ Consider using 'resourceAccessPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fceResourceAccessPolicies :: Lens.Lens' FunctionConfigurationEnvironment (Lude.Maybe [ResourceAccessPolicy])
fceResourceAccessPolicies = Lens.lens (resourceAccessPolicies :: FunctionConfigurationEnvironment -> Lude.Maybe [ResourceAccessPolicy]) (\s a -> s {resourceAccessPolicies = a} :: FunctionConfigurationEnvironment)
{-# DEPRECATED fceResourceAccessPolicies "Use generic-lens or generic-optics with 'resourceAccessPolicies' instead." #-}

-- | If true, the Lambda function is allowed to access the host's /sys folder. Use this when the Lambda function needs to read device information from /sys. This setting applies only when you run the Lambda function in a Greengrass container.
--
-- /Note:/ Consider using 'accessSysfs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fceAccessSysfs :: Lens.Lens' FunctionConfigurationEnvironment (Lude.Maybe Lude.Bool)
fceAccessSysfs = Lens.lens (accessSysfs :: FunctionConfigurationEnvironment -> Lude.Maybe Lude.Bool) (\s a -> s {accessSysfs = a} :: FunctionConfigurationEnvironment)
{-# DEPRECATED fceAccessSysfs "Use generic-lens or generic-optics with 'accessSysfs' instead." #-}

instance Lude.FromJSON FunctionConfigurationEnvironment where
  parseJSON =
    Lude.withObject
      "FunctionConfigurationEnvironment"
      ( \x ->
          FunctionConfigurationEnvironment'
            Lude.<$> (x Lude..:? "Variables" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Execution")
            Lude.<*> (x Lude..:? "ResourceAccessPolicies" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "AccessSysfs")
      )

instance Lude.ToJSON FunctionConfigurationEnvironment where
  toJSON FunctionConfigurationEnvironment' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Variables" Lude..=) Lude.<$> variables,
            ("Execution" Lude..=) Lude.<$> execution,
            ("ResourceAccessPolicies" Lude..=) Lude.<$> resourceAccessPolicies,
            ("AccessSysfs" Lude..=) Lude.<$> accessSysfs
          ]
      )
