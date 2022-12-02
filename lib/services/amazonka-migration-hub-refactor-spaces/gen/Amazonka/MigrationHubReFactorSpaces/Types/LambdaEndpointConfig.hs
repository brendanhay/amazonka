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
-- Module      : Amazonka.MigrationHubReFactorSpaces.Types.LambdaEndpointConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubReFactorSpaces.Types.LambdaEndpointConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration for the Lambda endpoint type.
--
-- /See:/ 'newLambdaEndpointConfig' smart constructor.
data LambdaEndpointConfig = LambdaEndpointConfig'
  { -- | The Amazon Resource Name (ARN) of the Lambda endpoint.
    arn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaEndpointConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'lambdaEndpointConfig_arn' - The Amazon Resource Name (ARN) of the Lambda endpoint.
newLambdaEndpointConfig ::
  LambdaEndpointConfig
newLambdaEndpointConfig =
  LambdaEndpointConfig' {arn = Prelude.Nothing}

-- | The Amazon Resource Name (ARN) of the Lambda endpoint.
lambdaEndpointConfig_arn :: Lens.Lens' LambdaEndpointConfig (Prelude.Maybe Prelude.Text)
lambdaEndpointConfig_arn = Lens.lens (\LambdaEndpointConfig' {arn} -> arn) (\s@LambdaEndpointConfig' {} a -> s {arn = a} :: LambdaEndpointConfig)

instance Data.FromJSON LambdaEndpointConfig where
  parseJSON =
    Data.withObject
      "LambdaEndpointConfig"
      ( \x ->
          LambdaEndpointConfig' Prelude.<$> (x Data..:? "Arn")
      )

instance Prelude.Hashable LambdaEndpointConfig where
  hashWithSalt _salt LambdaEndpointConfig' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData LambdaEndpointConfig where
  rnf LambdaEndpointConfig' {..} = Prelude.rnf arn
