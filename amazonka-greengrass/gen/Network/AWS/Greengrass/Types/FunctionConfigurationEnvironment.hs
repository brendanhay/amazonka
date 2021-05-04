{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.Greengrass.Types.FunctionExecutionConfig
import Network.AWS.Greengrass.Types.ResourceAccessPolicy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The environment configuration of the function.
--
-- /See:/ 'newFunctionConfigurationEnvironment' smart constructor.
data FunctionConfigurationEnvironment = FunctionConfigurationEnvironment'
  { -- | If true, the Lambda function is allowed to access the host\'s \/sys
    -- folder. Use this when the Lambda function needs to read device
    -- information from \/sys. This setting applies only when you run the
    -- Lambda function in a Greengrass container.
    accessSysfs :: Prelude.Maybe Prelude.Bool,
    -- | Environment variables for the Lambda function\'s configuration.
    variables :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Configuration related to executing the Lambda function
    execution :: Prelude.Maybe FunctionExecutionConfig,
    -- | A list of the resources, with their permissions, to which the Lambda
    -- function will be granted access. A Lambda function can have at most 10
    -- resources. ResourceAccessPolicies apply only when you run the Lambda
    -- function in a Greengrass container.
    resourceAccessPolicies :: Prelude.Maybe [ResourceAccessPolicy]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      variables = Prelude.Nothing,
      execution = Prelude.Nothing,
      resourceAccessPolicies = Prelude.Nothing
    }

-- | If true, the Lambda function is allowed to access the host\'s \/sys
-- folder. Use this when the Lambda function needs to read device
-- information from \/sys. This setting applies only when you run the
-- Lambda function in a Greengrass container.
functionConfigurationEnvironment_accessSysfs :: Lens.Lens' FunctionConfigurationEnvironment (Prelude.Maybe Prelude.Bool)
functionConfigurationEnvironment_accessSysfs = Lens.lens (\FunctionConfigurationEnvironment' {accessSysfs} -> accessSysfs) (\s@FunctionConfigurationEnvironment' {} a -> s {accessSysfs = a} :: FunctionConfigurationEnvironment)

-- | Environment variables for the Lambda function\'s configuration.
functionConfigurationEnvironment_variables :: Lens.Lens' FunctionConfigurationEnvironment (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
functionConfigurationEnvironment_variables = Lens.lens (\FunctionConfigurationEnvironment' {variables} -> variables) (\s@FunctionConfigurationEnvironment' {} a -> s {variables = a} :: FunctionConfigurationEnvironment) Prelude.. Lens.mapping Prelude._Coerce

-- | Configuration related to executing the Lambda function
functionConfigurationEnvironment_execution :: Lens.Lens' FunctionConfigurationEnvironment (Prelude.Maybe FunctionExecutionConfig)
functionConfigurationEnvironment_execution = Lens.lens (\FunctionConfigurationEnvironment' {execution} -> execution) (\s@FunctionConfigurationEnvironment' {} a -> s {execution = a} :: FunctionConfigurationEnvironment)

-- | A list of the resources, with their permissions, to which the Lambda
-- function will be granted access. A Lambda function can have at most 10
-- resources. ResourceAccessPolicies apply only when you run the Lambda
-- function in a Greengrass container.
functionConfigurationEnvironment_resourceAccessPolicies :: Lens.Lens' FunctionConfigurationEnvironment (Prelude.Maybe [ResourceAccessPolicy])
functionConfigurationEnvironment_resourceAccessPolicies = Lens.lens (\FunctionConfigurationEnvironment' {resourceAccessPolicies} -> resourceAccessPolicies) (\s@FunctionConfigurationEnvironment' {} a -> s {resourceAccessPolicies = a} :: FunctionConfigurationEnvironment) Prelude.. Lens.mapping Prelude._Coerce

instance
  Prelude.FromJSON
    FunctionConfigurationEnvironment
  where
  parseJSON =
    Prelude.withObject
      "FunctionConfigurationEnvironment"
      ( \x ->
          FunctionConfigurationEnvironment'
            Prelude.<$> (x Prelude..:? "AccessSysfs")
            Prelude.<*> ( x Prelude..:? "Variables"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "Execution")
            Prelude.<*> ( x Prelude..:? "ResourceAccessPolicies"
                            Prelude..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    FunctionConfigurationEnvironment

instance
  Prelude.NFData
    FunctionConfigurationEnvironment

instance
  Prelude.ToJSON
    FunctionConfigurationEnvironment
  where
  toJSON FunctionConfigurationEnvironment' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("AccessSysfs" Prelude..=) Prelude.<$> accessSysfs,
            ("Variables" Prelude..=) Prelude.<$> variables,
            ("Execution" Prelude..=) Prelude.<$> execution,
            ("ResourceAccessPolicies" Prelude..=)
              Prelude.<$> resourceAccessPolicies
          ]
      )
