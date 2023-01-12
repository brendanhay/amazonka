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
-- Module      : Amazonka.IoTSiteWise.Types.Transform
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.Transform where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.ExpressionVariable
import Amazonka.IoTSiteWise.Types.TransformProcessingConfig
import qualified Amazonka.Prelude as Prelude

-- | Contains an asset transform property. A transform is a one-to-one
-- mapping of a property\'s data points from one form to another. For
-- example, you can use a transform to convert a Celsius data stream to
-- Fahrenheit by applying the transformation expression to each data point
-- of the Celsius stream. A transform can only have a data type of @DOUBLE@
-- and consume properties with data types of @INTEGER@ or @DOUBLE@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/asset-properties.html#transforms Transforms>
-- in the /IoT SiteWise User Guide/.
--
-- /See:/ 'newTransform' smart constructor.
data Transform = Transform'
  { -- | The processing configuration for the given transform property. You can
    -- configure transforms to be kept at the edge or forwarded to the Amazon
    -- Web Services Cloud. You can also configure transforms to be computed at
    -- the edge or in the cloud.
    processingConfig :: Prelude.Maybe TransformProcessingConfig,
    -- | The mathematical expression that defines the transformation function.
    -- You can specify up to 10 variables per expression. You can specify up to
    -- 10 functions per expression.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/quotas.html Quotas>
    -- in the /IoT SiteWise User Guide/.
    expression :: Prelude.Text,
    -- | The list of variables used in the expression.
    variables :: [ExpressionVariable]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Transform' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'processingConfig', 'transform_processingConfig' - The processing configuration for the given transform property. You can
-- configure transforms to be kept at the edge or forwarded to the Amazon
-- Web Services Cloud. You can also configure transforms to be computed at
-- the edge or in the cloud.
--
-- 'expression', 'transform_expression' - The mathematical expression that defines the transformation function.
-- You can specify up to 10 variables per expression. You can specify up to
-- 10 functions per expression.
--
-- For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/quotas.html Quotas>
-- in the /IoT SiteWise User Guide/.
--
-- 'variables', 'transform_variables' - The list of variables used in the expression.
newTransform ::
  -- | 'expression'
  Prelude.Text ->
  Transform
newTransform pExpression_ =
  Transform'
    { processingConfig = Prelude.Nothing,
      expression = pExpression_,
      variables = Prelude.mempty
    }

-- | The processing configuration for the given transform property. You can
-- configure transforms to be kept at the edge or forwarded to the Amazon
-- Web Services Cloud. You can also configure transforms to be computed at
-- the edge or in the cloud.
transform_processingConfig :: Lens.Lens' Transform (Prelude.Maybe TransformProcessingConfig)
transform_processingConfig = Lens.lens (\Transform' {processingConfig} -> processingConfig) (\s@Transform' {} a -> s {processingConfig = a} :: Transform)

-- | The mathematical expression that defines the transformation function.
-- You can specify up to 10 variables per expression. You can specify up to
-- 10 functions per expression.
--
-- For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/quotas.html Quotas>
-- in the /IoT SiteWise User Guide/.
transform_expression :: Lens.Lens' Transform Prelude.Text
transform_expression = Lens.lens (\Transform' {expression} -> expression) (\s@Transform' {} a -> s {expression = a} :: Transform)

-- | The list of variables used in the expression.
transform_variables :: Lens.Lens' Transform [ExpressionVariable]
transform_variables = Lens.lens (\Transform' {variables} -> variables) (\s@Transform' {} a -> s {variables = a} :: Transform) Prelude.. Lens.coerced

instance Data.FromJSON Transform where
  parseJSON =
    Data.withObject
      "Transform"
      ( \x ->
          Transform'
            Prelude.<$> (x Data..:? "processingConfig")
            Prelude.<*> (x Data..: "expression")
            Prelude.<*> (x Data..:? "variables" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Transform where
  hashWithSalt _salt Transform' {..} =
    _salt `Prelude.hashWithSalt` processingConfig
      `Prelude.hashWithSalt` expression
      `Prelude.hashWithSalt` variables

instance Prelude.NFData Transform where
  rnf Transform' {..} =
    Prelude.rnf processingConfig
      `Prelude.seq` Prelude.rnf expression
      `Prelude.seq` Prelude.rnf variables

instance Data.ToJSON Transform where
  toJSON Transform' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("processingConfig" Data..=)
              Prelude.<$> processingConfig,
            Prelude.Just ("expression" Data..= expression),
            Prelude.Just ("variables" Data..= variables)
          ]
      )
