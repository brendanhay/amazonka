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
-- Module      : Amazonka.Greengrass.Types.FunctionConfigurationEnvironment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.FunctionConfigurationEnvironment where

import qualified Amazonka.Core as Core
import Amazonka.Greengrass.Types.FunctionExecutionConfig
import Amazonka.Greengrass.Types.ResourceAccessPolicy
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The environment configuration of the function.
--
-- /See:/ 'newFunctionConfigurationEnvironment' smart constructor.
data FunctionConfigurationEnvironment = FunctionConfigurationEnvironment'
  { -- | Environment variables for the Lambda function\'s configuration.
    variables :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Configuration related to executing the Lambda function
    execution :: Prelude.Maybe FunctionExecutionConfig,
    -- | A list of the resources, with their permissions, to which the Lambda
    -- function will be granted access. A Lambda function can have at most 10
    -- resources. ResourceAccessPolicies apply only when you run the Lambda
    -- function in a Greengrass container.
    resourceAccessPolicies :: Prelude.Maybe [ResourceAccessPolicy],
    -- | If true, the Lambda function is allowed to access the host\'s \/sys
    -- folder. Use this when the Lambda function needs to read device
    -- information from \/sys. This setting applies only when you run the
    -- Lambda function in a Greengrass container.
    accessSysfs :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FunctionConfigurationEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'variables', 'functionConfigurationEnvironment_variables' - Environment variables for the Lambda function\'s configuration.
--
-- 'execution', 'functionConfigurationEnvironment_execution' - Configuration related to executing the Lambda function
--
-- 'resourceAccessPolicies', 'functionConfigurationEnvironment_resourceAccessPolicies' - A list of the resources, with their permissions, to which the Lambda
-- function will be granted access. A Lambda function can have at most 10
-- resources. ResourceAccessPolicies apply only when you run the Lambda
-- function in a Greengrass container.
--
-- 'accessSysfs', 'functionConfigurationEnvironment_accessSysfs' - If true, the Lambda function is allowed to access the host\'s \/sys
-- folder. Use this when the Lambda function needs to read device
-- information from \/sys. This setting applies only when you run the
-- Lambda function in a Greengrass container.
newFunctionConfigurationEnvironment ::
  FunctionConfigurationEnvironment
newFunctionConfigurationEnvironment =
  FunctionConfigurationEnvironment'
    { variables =
        Prelude.Nothing,
      execution = Prelude.Nothing,
      resourceAccessPolicies = Prelude.Nothing,
      accessSysfs = Prelude.Nothing
    }

-- | Environment variables for the Lambda function\'s configuration.
functionConfigurationEnvironment_variables :: Lens.Lens' FunctionConfigurationEnvironment (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
functionConfigurationEnvironment_variables = Lens.lens (\FunctionConfigurationEnvironment' {variables} -> variables) (\s@FunctionConfigurationEnvironment' {} a -> s {variables = a} :: FunctionConfigurationEnvironment) Prelude.. Lens.mapping Lens.coerced

-- | Configuration related to executing the Lambda function
functionConfigurationEnvironment_execution :: Lens.Lens' FunctionConfigurationEnvironment (Prelude.Maybe FunctionExecutionConfig)
functionConfigurationEnvironment_execution = Lens.lens (\FunctionConfigurationEnvironment' {execution} -> execution) (\s@FunctionConfigurationEnvironment' {} a -> s {execution = a} :: FunctionConfigurationEnvironment)

-- | A list of the resources, with their permissions, to which the Lambda
-- function will be granted access. A Lambda function can have at most 10
-- resources. ResourceAccessPolicies apply only when you run the Lambda
-- function in a Greengrass container.
functionConfigurationEnvironment_resourceAccessPolicies :: Lens.Lens' FunctionConfigurationEnvironment (Prelude.Maybe [ResourceAccessPolicy])
functionConfigurationEnvironment_resourceAccessPolicies = Lens.lens (\FunctionConfigurationEnvironment' {resourceAccessPolicies} -> resourceAccessPolicies) (\s@FunctionConfigurationEnvironment' {} a -> s {resourceAccessPolicies = a} :: FunctionConfigurationEnvironment) Prelude.. Lens.mapping Lens.coerced

-- | If true, the Lambda function is allowed to access the host\'s \/sys
-- folder. Use this when the Lambda function needs to read device
-- information from \/sys. This setting applies only when you run the
-- Lambda function in a Greengrass container.
functionConfigurationEnvironment_accessSysfs :: Lens.Lens' FunctionConfigurationEnvironment (Prelude.Maybe Prelude.Bool)
functionConfigurationEnvironment_accessSysfs = Lens.lens (\FunctionConfigurationEnvironment' {accessSysfs} -> accessSysfs) (\s@FunctionConfigurationEnvironment' {} a -> s {accessSysfs = a} :: FunctionConfigurationEnvironment)

instance
  Core.FromJSON
    FunctionConfigurationEnvironment
  where
  parseJSON =
    Core.withObject
      "FunctionConfigurationEnvironment"
      ( \x ->
          FunctionConfigurationEnvironment'
            Prelude.<$> (x Core..:? "Variables" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Execution")
            Prelude.<*> ( x Core..:? "ResourceAccessPolicies"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "AccessSysfs")
      )

instance
  Prelude.Hashable
    FunctionConfigurationEnvironment

instance
  Prelude.NFData
    FunctionConfigurationEnvironment

instance Core.ToJSON FunctionConfigurationEnvironment where
  toJSON FunctionConfigurationEnvironment' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Variables" Core..=) Prelude.<$> variables,
            ("Execution" Core..=) Prelude.<$> execution,
            ("ResourceAccessPolicies" Core..=)
              Prelude.<$> resourceAccessPolicies,
            ("AccessSysfs" Core..=) Prelude.<$> accessSysfs
          ]
      )
