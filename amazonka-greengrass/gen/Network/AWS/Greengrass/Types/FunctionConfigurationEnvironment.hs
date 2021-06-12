{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.FunctionConfigurationEnvironment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.FunctionConfigurationEnvironment where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types.FunctionExecutionConfig
import Network.AWS.Greengrass.Types.ResourceAccessPolicy
import qualified Network.AWS.Lens as Lens

-- | The environment configuration of the function.
--
-- /See:/ 'newFunctionConfigurationEnvironment' smart constructor.
data FunctionConfigurationEnvironment = FunctionConfigurationEnvironment'
  { -- | If true, the Lambda function is allowed to access the host\'s \/sys
    -- folder. Use this when the Lambda function needs to read device
    -- information from \/sys. This setting applies only when you run the
    -- Lambda function in a Greengrass container.
    accessSysfs :: Core.Maybe Core.Bool,
    -- | Environment variables for the Lambda function\'s configuration.
    variables :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Configuration related to executing the Lambda function
    execution :: Core.Maybe FunctionExecutionConfig,
    -- | A list of the resources, with their permissions, to which the Lambda
    -- function will be granted access. A Lambda function can have at most 10
    -- resources. ResourceAccessPolicies apply only when you run the Lambda
    -- function in a Greengrass container.
    resourceAccessPolicies :: Core.Maybe [ResourceAccessPolicy]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FunctionConfigurationEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessSysfs', 'functionConfigurationEnvironment_accessSysfs' - If true, the Lambda function is allowed to access the host\'s \/sys
-- folder. Use this when the Lambda function needs to read device
-- information from \/sys. This setting applies only when you run the
-- Lambda function in a Greengrass container.
--
-- 'variables', 'functionConfigurationEnvironment_variables' - Environment variables for the Lambda function\'s configuration.
--
-- 'execution', 'functionConfigurationEnvironment_execution' - Configuration related to executing the Lambda function
--
-- 'resourceAccessPolicies', 'functionConfigurationEnvironment_resourceAccessPolicies' - A list of the resources, with their permissions, to which the Lambda
-- function will be granted access. A Lambda function can have at most 10
-- resources. ResourceAccessPolicies apply only when you run the Lambda
-- function in a Greengrass container.
newFunctionConfigurationEnvironment ::
  FunctionConfigurationEnvironment
newFunctionConfigurationEnvironment =
  FunctionConfigurationEnvironment'
    { accessSysfs =
        Core.Nothing,
      variables = Core.Nothing,
      execution = Core.Nothing,
      resourceAccessPolicies = Core.Nothing
    }

-- | If true, the Lambda function is allowed to access the host\'s \/sys
-- folder. Use this when the Lambda function needs to read device
-- information from \/sys. This setting applies only when you run the
-- Lambda function in a Greengrass container.
functionConfigurationEnvironment_accessSysfs :: Lens.Lens' FunctionConfigurationEnvironment (Core.Maybe Core.Bool)
functionConfigurationEnvironment_accessSysfs = Lens.lens (\FunctionConfigurationEnvironment' {accessSysfs} -> accessSysfs) (\s@FunctionConfigurationEnvironment' {} a -> s {accessSysfs = a} :: FunctionConfigurationEnvironment)

-- | Environment variables for the Lambda function\'s configuration.
functionConfigurationEnvironment_variables :: Lens.Lens' FunctionConfigurationEnvironment (Core.Maybe (Core.HashMap Core.Text Core.Text))
functionConfigurationEnvironment_variables = Lens.lens (\FunctionConfigurationEnvironment' {variables} -> variables) (\s@FunctionConfigurationEnvironment' {} a -> s {variables = a} :: FunctionConfigurationEnvironment) Core.. Lens.mapping Lens._Coerce

-- | Configuration related to executing the Lambda function
functionConfigurationEnvironment_execution :: Lens.Lens' FunctionConfigurationEnvironment (Core.Maybe FunctionExecutionConfig)
functionConfigurationEnvironment_execution = Lens.lens (\FunctionConfigurationEnvironment' {execution} -> execution) (\s@FunctionConfigurationEnvironment' {} a -> s {execution = a} :: FunctionConfigurationEnvironment)

-- | A list of the resources, with their permissions, to which the Lambda
-- function will be granted access. A Lambda function can have at most 10
-- resources. ResourceAccessPolicies apply only when you run the Lambda
-- function in a Greengrass container.
functionConfigurationEnvironment_resourceAccessPolicies :: Lens.Lens' FunctionConfigurationEnvironment (Core.Maybe [ResourceAccessPolicy])
functionConfigurationEnvironment_resourceAccessPolicies = Lens.lens (\FunctionConfigurationEnvironment' {resourceAccessPolicies} -> resourceAccessPolicies) (\s@FunctionConfigurationEnvironment' {} a -> s {resourceAccessPolicies = a} :: FunctionConfigurationEnvironment) Core.. Lens.mapping Lens._Coerce

instance
  Core.FromJSON
    FunctionConfigurationEnvironment
  where
  parseJSON =
    Core.withObject
      "FunctionConfigurationEnvironment"
      ( \x ->
          FunctionConfigurationEnvironment'
            Core.<$> (x Core..:? "AccessSysfs")
            Core.<*> (x Core..:? "Variables" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Execution")
            Core.<*> ( x Core..:? "ResourceAccessPolicies"
                         Core..!= Core.mempty
                     )
      )

instance
  Core.Hashable
    FunctionConfigurationEnvironment

instance Core.NFData FunctionConfigurationEnvironment

instance Core.ToJSON FunctionConfigurationEnvironment where
  toJSON FunctionConfigurationEnvironment' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AccessSysfs" Core..=) Core.<$> accessSysfs,
            ("Variables" Core..=) Core.<$> variables,
            ("Execution" Core..=) Core.<$> execution,
            ("ResourceAccessPolicies" Core..=)
              Core.<$> resourceAccessPolicies
          ]
      )
