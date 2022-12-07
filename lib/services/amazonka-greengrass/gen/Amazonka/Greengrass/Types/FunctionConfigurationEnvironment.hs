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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.FunctionConfigurationEnvironment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types.FunctionExecutionConfig
import Amazonka.Greengrass.Types.ResourceAccessPolicy
import qualified Amazonka.Prelude as Prelude

-- | The environment configuration of the function.
--
-- /See:/ 'newFunctionConfigurationEnvironment' smart constructor.
data FunctionConfigurationEnvironment = FunctionConfigurationEnvironment'
  { -- | A list of the resources, with their permissions, to which the Lambda
    -- function will be granted access. A Lambda function can have at most 10
    -- resources. ResourceAccessPolicies apply only when you run the Lambda
    -- function in a Greengrass container.
    resourceAccessPolicies :: Prelude.Maybe [ResourceAccessPolicy],
    -- | Configuration related to executing the Lambda function
    execution :: Prelude.Maybe FunctionExecutionConfig,
    -- | If true, the Lambda function is allowed to access the host\'s \/sys
    -- folder. Use this when the Lambda function needs to read device
    -- information from \/sys. This setting applies only when you run the
    -- Lambda function in a Greengrass container.
    accessSysfs :: Prelude.Maybe Prelude.Bool,
    -- | Environment variables for the Lambda function\'s configuration.
    variables :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
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
-- 'resourceAccessPolicies', 'functionConfigurationEnvironment_resourceAccessPolicies' - A list of the resources, with their permissions, to which the Lambda
-- function will be granted access. A Lambda function can have at most 10
-- resources. ResourceAccessPolicies apply only when you run the Lambda
-- function in a Greengrass container.
--
-- 'execution', 'functionConfigurationEnvironment_execution' - Configuration related to executing the Lambda function
--
-- 'accessSysfs', 'functionConfigurationEnvironment_accessSysfs' - If true, the Lambda function is allowed to access the host\'s \/sys
-- folder. Use this when the Lambda function needs to read device
-- information from \/sys. This setting applies only when you run the
-- Lambda function in a Greengrass container.
--
-- 'variables', 'functionConfigurationEnvironment_variables' - Environment variables for the Lambda function\'s configuration.
newFunctionConfigurationEnvironment ::
  FunctionConfigurationEnvironment
newFunctionConfigurationEnvironment =
  FunctionConfigurationEnvironment'
    { resourceAccessPolicies =
        Prelude.Nothing,
      execution = Prelude.Nothing,
      accessSysfs = Prelude.Nothing,
      variables = Prelude.Nothing
    }

-- | A list of the resources, with their permissions, to which the Lambda
-- function will be granted access. A Lambda function can have at most 10
-- resources. ResourceAccessPolicies apply only when you run the Lambda
-- function in a Greengrass container.
functionConfigurationEnvironment_resourceAccessPolicies :: Lens.Lens' FunctionConfigurationEnvironment (Prelude.Maybe [ResourceAccessPolicy])
functionConfigurationEnvironment_resourceAccessPolicies = Lens.lens (\FunctionConfigurationEnvironment' {resourceAccessPolicies} -> resourceAccessPolicies) (\s@FunctionConfigurationEnvironment' {} a -> s {resourceAccessPolicies = a} :: FunctionConfigurationEnvironment) Prelude.. Lens.mapping Lens.coerced

-- | Configuration related to executing the Lambda function
functionConfigurationEnvironment_execution :: Lens.Lens' FunctionConfigurationEnvironment (Prelude.Maybe FunctionExecutionConfig)
functionConfigurationEnvironment_execution = Lens.lens (\FunctionConfigurationEnvironment' {execution} -> execution) (\s@FunctionConfigurationEnvironment' {} a -> s {execution = a} :: FunctionConfigurationEnvironment)

-- | If true, the Lambda function is allowed to access the host\'s \/sys
-- folder. Use this when the Lambda function needs to read device
-- information from \/sys. This setting applies only when you run the
-- Lambda function in a Greengrass container.
functionConfigurationEnvironment_accessSysfs :: Lens.Lens' FunctionConfigurationEnvironment (Prelude.Maybe Prelude.Bool)
functionConfigurationEnvironment_accessSysfs = Lens.lens (\FunctionConfigurationEnvironment' {accessSysfs} -> accessSysfs) (\s@FunctionConfigurationEnvironment' {} a -> s {accessSysfs = a} :: FunctionConfigurationEnvironment)

-- | Environment variables for the Lambda function\'s configuration.
functionConfigurationEnvironment_variables :: Lens.Lens' FunctionConfigurationEnvironment (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
functionConfigurationEnvironment_variables = Lens.lens (\FunctionConfigurationEnvironment' {variables} -> variables) (\s@FunctionConfigurationEnvironment' {} a -> s {variables = a} :: FunctionConfigurationEnvironment) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    FunctionConfigurationEnvironment
  where
  parseJSON =
    Data.withObject
      "FunctionConfigurationEnvironment"
      ( \x ->
          FunctionConfigurationEnvironment'
            Prelude.<$> ( x Data..:? "ResourceAccessPolicies"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Execution")
            Prelude.<*> (x Data..:? "AccessSysfs")
            Prelude.<*> (x Data..:? "Variables" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    FunctionConfigurationEnvironment
  where
  hashWithSalt
    _salt
    FunctionConfigurationEnvironment' {..} =
      _salt `Prelude.hashWithSalt` resourceAccessPolicies
        `Prelude.hashWithSalt` execution
        `Prelude.hashWithSalt` accessSysfs
        `Prelude.hashWithSalt` variables

instance
  Prelude.NFData
    FunctionConfigurationEnvironment
  where
  rnf FunctionConfigurationEnvironment' {..} =
    Prelude.rnf resourceAccessPolicies
      `Prelude.seq` Prelude.rnf execution
      `Prelude.seq` Prelude.rnf accessSysfs
      `Prelude.seq` Prelude.rnf variables

instance Data.ToJSON FunctionConfigurationEnvironment where
  toJSON FunctionConfigurationEnvironment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ResourceAccessPolicies" Data..=)
              Prelude.<$> resourceAccessPolicies,
            ("Execution" Data..=) Prelude.<$> execution,
            ("AccessSysfs" Data..=) Prelude.<$> accessSysfs,
            ("Variables" Data..=) Prelude.<$> variables
          ]
      )
