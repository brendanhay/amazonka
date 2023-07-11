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
-- Module      : Amazonka.Inspector2.Types.LambdaFunctionMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.LambdaFunctionMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.Runtime
import qualified Amazonka.Prelude as Prelude

-- | The AWS Lambda function metadata.
--
-- /See:/ 'newLambdaFunctionMetadata' smart constructor.
data LambdaFunctionMetadata = LambdaFunctionMetadata'
  { -- | The name of a function.
    functionName :: Prelude.Maybe Prelude.Text,
    -- | The resource tags on an AWS Lambda function.
    functionTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The layers for an AWS Lambda function. A Lambda function can have up to
    -- five layers.
    layers :: Prelude.Maybe [Prelude.Text],
    -- | An AWS Lambda function\'s runtime.
    runtime :: Prelude.Maybe Runtime
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaFunctionMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'functionName', 'lambdaFunctionMetadata_functionName' - The name of a function.
--
-- 'functionTags', 'lambdaFunctionMetadata_functionTags' - The resource tags on an AWS Lambda function.
--
-- 'layers', 'lambdaFunctionMetadata_layers' - The layers for an AWS Lambda function. A Lambda function can have up to
-- five layers.
--
-- 'runtime', 'lambdaFunctionMetadata_runtime' - An AWS Lambda function\'s runtime.
newLambdaFunctionMetadata ::
  LambdaFunctionMetadata
newLambdaFunctionMetadata =
  LambdaFunctionMetadata'
    { functionName =
        Prelude.Nothing,
      functionTags = Prelude.Nothing,
      layers = Prelude.Nothing,
      runtime = Prelude.Nothing
    }

-- | The name of a function.
lambdaFunctionMetadata_functionName :: Lens.Lens' LambdaFunctionMetadata (Prelude.Maybe Prelude.Text)
lambdaFunctionMetadata_functionName = Lens.lens (\LambdaFunctionMetadata' {functionName} -> functionName) (\s@LambdaFunctionMetadata' {} a -> s {functionName = a} :: LambdaFunctionMetadata)

-- | The resource tags on an AWS Lambda function.
lambdaFunctionMetadata_functionTags :: Lens.Lens' LambdaFunctionMetadata (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
lambdaFunctionMetadata_functionTags = Lens.lens (\LambdaFunctionMetadata' {functionTags} -> functionTags) (\s@LambdaFunctionMetadata' {} a -> s {functionTags = a} :: LambdaFunctionMetadata) Prelude.. Lens.mapping Lens.coerced

-- | The layers for an AWS Lambda function. A Lambda function can have up to
-- five layers.
lambdaFunctionMetadata_layers :: Lens.Lens' LambdaFunctionMetadata (Prelude.Maybe [Prelude.Text])
lambdaFunctionMetadata_layers = Lens.lens (\LambdaFunctionMetadata' {layers} -> layers) (\s@LambdaFunctionMetadata' {} a -> s {layers = a} :: LambdaFunctionMetadata) Prelude.. Lens.mapping Lens.coerced

-- | An AWS Lambda function\'s runtime.
lambdaFunctionMetadata_runtime :: Lens.Lens' LambdaFunctionMetadata (Prelude.Maybe Runtime)
lambdaFunctionMetadata_runtime = Lens.lens (\LambdaFunctionMetadata' {runtime} -> runtime) (\s@LambdaFunctionMetadata' {} a -> s {runtime = a} :: LambdaFunctionMetadata)

instance Data.FromJSON LambdaFunctionMetadata where
  parseJSON =
    Data.withObject
      "LambdaFunctionMetadata"
      ( \x ->
          LambdaFunctionMetadata'
            Prelude.<$> (x Data..:? "functionName")
            Prelude.<*> (x Data..:? "functionTags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "layers" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "runtime")
      )

instance Prelude.Hashable LambdaFunctionMetadata where
  hashWithSalt _salt LambdaFunctionMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` functionName
      `Prelude.hashWithSalt` functionTags
      `Prelude.hashWithSalt` layers
      `Prelude.hashWithSalt` runtime

instance Prelude.NFData LambdaFunctionMetadata where
  rnf LambdaFunctionMetadata' {..} =
    Prelude.rnf functionName
      `Prelude.seq` Prelude.rnf functionTags
      `Prelude.seq` Prelude.rnf layers
      `Prelude.seq` Prelude.rnf runtime
