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
-- Module      : Amazonka.MigrationHubReFactorSpaces.Types.LambdaEndpointSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubReFactorSpaces.Types.LambdaEndpointSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The summary for the Lambda endpoint type.
--
-- /See:/ 'newLambdaEndpointSummary' smart constructor.
data LambdaEndpointSummary = LambdaEndpointSummary'
  { -- | The Amazon Resource Name (ARN) of the Lambda endpoint.
    arn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaEndpointSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'lambdaEndpointSummary_arn' - The Amazon Resource Name (ARN) of the Lambda endpoint.
newLambdaEndpointSummary ::
  LambdaEndpointSummary
newLambdaEndpointSummary =
  LambdaEndpointSummary' {arn = Prelude.Nothing}

-- | The Amazon Resource Name (ARN) of the Lambda endpoint.
lambdaEndpointSummary_arn :: Lens.Lens' LambdaEndpointSummary (Prelude.Maybe Prelude.Text)
lambdaEndpointSummary_arn = Lens.lens (\LambdaEndpointSummary' {arn} -> arn) (\s@LambdaEndpointSummary' {} a -> s {arn = a} :: LambdaEndpointSummary)

instance Core.FromJSON LambdaEndpointSummary where
  parseJSON =
    Core.withObject
      "LambdaEndpointSummary"
      ( \x ->
          LambdaEndpointSummary'
            Prelude.<$> (x Core..:? "Arn")
      )

instance Prelude.Hashable LambdaEndpointSummary where
  hashWithSalt _salt LambdaEndpointSummary' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData LambdaEndpointSummary where
  rnf LambdaEndpointSummary' {..} = Prelude.rnf arn
