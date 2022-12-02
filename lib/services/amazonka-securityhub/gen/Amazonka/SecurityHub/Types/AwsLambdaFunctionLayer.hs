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
-- Module      : Amazonka.SecurityHub.Types.AwsLambdaFunctionLayer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsLambdaFunctionLayer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An Lambda layer.
--
-- /See:/ 'newAwsLambdaFunctionLayer' smart constructor.
data AwsLambdaFunctionLayer = AwsLambdaFunctionLayer'
  { -- | The ARN of the function layer.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The size of the layer archive in bytes.
    codeSize :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsLambdaFunctionLayer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'awsLambdaFunctionLayer_arn' - The ARN of the function layer.
--
-- 'codeSize', 'awsLambdaFunctionLayer_codeSize' - The size of the layer archive in bytes.
newAwsLambdaFunctionLayer ::
  AwsLambdaFunctionLayer
newAwsLambdaFunctionLayer =
  AwsLambdaFunctionLayer'
    { arn = Prelude.Nothing,
      codeSize = Prelude.Nothing
    }

-- | The ARN of the function layer.
awsLambdaFunctionLayer_arn :: Lens.Lens' AwsLambdaFunctionLayer (Prelude.Maybe Prelude.Text)
awsLambdaFunctionLayer_arn = Lens.lens (\AwsLambdaFunctionLayer' {arn} -> arn) (\s@AwsLambdaFunctionLayer' {} a -> s {arn = a} :: AwsLambdaFunctionLayer)

-- | The size of the layer archive in bytes.
awsLambdaFunctionLayer_codeSize :: Lens.Lens' AwsLambdaFunctionLayer (Prelude.Maybe Prelude.Int)
awsLambdaFunctionLayer_codeSize = Lens.lens (\AwsLambdaFunctionLayer' {codeSize} -> codeSize) (\s@AwsLambdaFunctionLayer' {} a -> s {codeSize = a} :: AwsLambdaFunctionLayer)

instance Data.FromJSON AwsLambdaFunctionLayer where
  parseJSON =
    Data.withObject
      "AwsLambdaFunctionLayer"
      ( \x ->
          AwsLambdaFunctionLayer'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CodeSize")
      )

instance Prelude.Hashable AwsLambdaFunctionLayer where
  hashWithSalt _salt AwsLambdaFunctionLayer' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` codeSize

instance Prelude.NFData AwsLambdaFunctionLayer where
  rnf AwsLambdaFunctionLayer' {..} =
    Prelude.rnf arn `Prelude.seq` Prelude.rnf codeSize

instance Data.ToJSON AwsLambdaFunctionLayer where
  toJSON AwsLambdaFunctionLayer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Arn" Data..=) Prelude.<$> arn,
            ("CodeSize" Data..=) Prelude.<$> codeSize
          ]
      )
