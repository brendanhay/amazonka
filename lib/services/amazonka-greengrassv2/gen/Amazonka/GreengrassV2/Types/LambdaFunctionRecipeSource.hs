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
-- Module      : Amazonka.GreengrassV2.Types.LambdaFunctionRecipeSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.LambdaFunctionRecipeSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GreengrassV2.Types.ComponentDependencyRequirement
import Amazonka.GreengrassV2.Types.ComponentPlatform
import Amazonka.GreengrassV2.Types.LambdaExecutionParameters
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an Lambda function to import to create a
-- component.
--
-- /See:/ 'newLambdaFunctionRecipeSource' smart constructor.
data LambdaFunctionRecipeSource = LambdaFunctionRecipeSource'
  { -- | The component versions on which this Lambda function component depends.
    componentDependencies :: Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentDependencyRequirement),
    -- | The system and runtime parameters for the Lambda function as it runs on
    -- the Greengrass core device.
    componentLambdaParameters :: Prelude.Maybe LambdaExecutionParameters,
    -- | The name of the component.
    --
    -- Defaults to the name of the Lambda function.
    componentName :: Prelude.Maybe Prelude.Text,
    -- | The platforms that the component version supports.
    componentPlatforms :: Prelude.Maybe [ComponentPlatform],
    -- | The version of the component.
    --
    -- Defaults to the version of the Lambda function as a semantic version.
    -- For example, if your function version is @3@, the component version
    -- becomes @3.0.0@.
    componentVersion :: Prelude.Maybe Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the Lambda function. The ARN must include the version of the function
    -- to import. You can\'t use version aliases like @$LATEST@.
    lambdaArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaFunctionRecipeSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'componentDependencies', 'lambdaFunctionRecipeSource_componentDependencies' - The component versions on which this Lambda function component depends.
--
-- 'componentLambdaParameters', 'lambdaFunctionRecipeSource_componentLambdaParameters' - The system and runtime parameters for the Lambda function as it runs on
-- the Greengrass core device.
--
-- 'componentName', 'lambdaFunctionRecipeSource_componentName' - The name of the component.
--
-- Defaults to the name of the Lambda function.
--
-- 'componentPlatforms', 'lambdaFunctionRecipeSource_componentPlatforms' - The platforms that the component version supports.
--
-- 'componentVersion', 'lambdaFunctionRecipeSource_componentVersion' - The version of the component.
--
-- Defaults to the version of the Lambda function as a semantic version.
-- For example, if your function version is @3@, the component version
-- becomes @3.0.0@.
--
-- 'lambdaArn', 'lambdaFunctionRecipeSource_lambdaArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the Lambda function. The ARN must include the version of the function
-- to import. You can\'t use version aliases like @$LATEST@.
newLambdaFunctionRecipeSource ::
  -- | 'lambdaArn'
  Prelude.Text ->
  LambdaFunctionRecipeSource
newLambdaFunctionRecipeSource pLambdaArn_ =
  LambdaFunctionRecipeSource'
    { componentDependencies =
        Prelude.Nothing,
      componentLambdaParameters = Prelude.Nothing,
      componentName = Prelude.Nothing,
      componentPlatforms = Prelude.Nothing,
      componentVersion = Prelude.Nothing,
      lambdaArn = pLambdaArn_
    }

-- | The component versions on which this Lambda function component depends.
lambdaFunctionRecipeSource_componentDependencies :: Lens.Lens' LambdaFunctionRecipeSource (Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentDependencyRequirement))
lambdaFunctionRecipeSource_componentDependencies = Lens.lens (\LambdaFunctionRecipeSource' {componentDependencies} -> componentDependencies) (\s@LambdaFunctionRecipeSource' {} a -> s {componentDependencies = a} :: LambdaFunctionRecipeSource) Prelude.. Lens.mapping Lens.coerced

-- | The system and runtime parameters for the Lambda function as it runs on
-- the Greengrass core device.
lambdaFunctionRecipeSource_componentLambdaParameters :: Lens.Lens' LambdaFunctionRecipeSource (Prelude.Maybe LambdaExecutionParameters)
lambdaFunctionRecipeSource_componentLambdaParameters = Lens.lens (\LambdaFunctionRecipeSource' {componentLambdaParameters} -> componentLambdaParameters) (\s@LambdaFunctionRecipeSource' {} a -> s {componentLambdaParameters = a} :: LambdaFunctionRecipeSource)

-- | The name of the component.
--
-- Defaults to the name of the Lambda function.
lambdaFunctionRecipeSource_componentName :: Lens.Lens' LambdaFunctionRecipeSource (Prelude.Maybe Prelude.Text)
lambdaFunctionRecipeSource_componentName = Lens.lens (\LambdaFunctionRecipeSource' {componentName} -> componentName) (\s@LambdaFunctionRecipeSource' {} a -> s {componentName = a} :: LambdaFunctionRecipeSource)

-- | The platforms that the component version supports.
lambdaFunctionRecipeSource_componentPlatforms :: Lens.Lens' LambdaFunctionRecipeSource (Prelude.Maybe [ComponentPlatform])
lambdaFunctionRecipeSource_componentPlatforms = Lens.lens (\LambdaFunctionRecipeSource' {componentPlatforms} -> componentPlatforms) (\s@LambdaFunctionRecipeSource' {} a -> s {componentPlatforms = a} :: LambdaFunctionRecipeSource) Prelude.. Lens.mapping Lens.coerced

-- | The version of the component.
--
-- Defaults to the version of the Lambda function as a semantic version.
-- For example, if your function version is @3@, the component version
-- becomes @3.0.0@.
lambdaFunctionRecipeSource_componentVersion :: Lens.Lens' LambdaFunctionRecipeSource (Prelude.Maybe Prelude.Text)
lambdaFunctionRecipeSource_componentVersion = Lens.lens (\LambdaFunctionRecipeSource' {componentVersion} -> componentVersion) (\s@LambdaFunctionRecipeSource' {} a -> s {componentVersion = a} :: LambdaFunctionRecipeSource)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the Lambda function. The ARN must include the version of the function
-- to import. You can\'t use version aliases like @$LATEST@.
lambdaFunctionRecipeSource_lambdaArn :: Lens.Lens' LambdaFunctionRecipeSource Prelude.Text
lambdaFunctionRecipeSource_lambdaArn = Lens.lens (\LambdaFunctionRecipeSource' {lambdaArn} -> lambdaArn) (\s@LambdaFunctionRecipeSource' {} a -> s {lambdaArn = a} :: LambdaFunctionRecipeSource)

instance Prelude.Hashable LambdaFunctionRecipeSource where
  hashWithSalt _salt LambdaFunctionRecipeSource' {..} =
    _salt `Prelude.hashWithSalt` componentDependencies
      `Prelude.hashWithSalt` componentLambdaParameters
      `Prelude.hashWithSalt` componentName
      `Prelude.hashWithSalt` componentPlatforms
      `Prelude.hashWithSalt` componentVersion
      `Prelude.hashWithSalt` lambdaArn

instance Prelude.NFData LambdaFunctionRecipeSource where
  rnf LambdaFunctionRecipeSource' {..} =
    Prelude.rnf componentDependencies
      `Prelude.seq` Prelude.rnf componentLambdaParameters
      `Prelude.seq` Prelude.rnf componentName
      `Prelude.seq` Prelude.rnf componentPlatforms
      `Prelude.seq` Prelude.rnf componentVersion
      `Prelude.seq` Prelude.rnf lambdaArn

instance Data.ToJSON LambdaFunctionRecipeSource where
  toJSON LambdaFunctionRecipeSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("componentDependencies" Data..=)
              Prelude.<$> componentDependencies,
            ("componentLambdaParameters" Data..=)
              Prelude.<$> componentLambdaParameters,
            ("componentName" Data..=) Prelude.<$> componentName,
            ("componentPlatforms" Data..=)
              Prelude.<$> componentPlatforms,
            ("componentVersion" Data..=)
              Prelude.<$> componentVersion,
            Prelude.Just ("lambdaArn" Data..= lambdaArn)
          ]
      )
