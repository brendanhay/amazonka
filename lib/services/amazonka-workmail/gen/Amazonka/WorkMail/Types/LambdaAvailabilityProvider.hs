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
-- Module      : Amazonka.WorkMail.Types.LambdaAvailabilityProvider
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMail.Types.LambdaAvailabilityProvider where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a Lambda based availability provider.
--
-- /See:/ 'newLambdaAvailabilityProvider' smart constructor.
data LambdaAvailabilityProvider = LambdaAvailabilityProvider'
  { -- | The Amazon Resource Name (ARN) of the Lambda that acts as the
    -- availability provider.
    lambdaArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaAvailabilityProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lambdaArn', 'lambdaAvailabilityProvider_lambdaArn' - The Amazon Resource Name (ARN) of the Lambda that acts as the
-- availability provider.
newLambdaAvailabilityProvider ::
  -- | 'lambdaArn'
  Prelude.Text ->
  LambdaAvailabilityProvider
newLambdaAvailabilityProvider pLambdaArn_ =
  LambdaAvailabilityProvider'
    { lambdaArn =
        pLambdaArn_
    }

-- | The Amazon Resource Name (ARN) of the Lambda that acts as the
-- availability provider.
lambdaAvailabilityProvider_lambdaArn :: Lens.Lens' LambdaAvailabilityProvider Prelude.Text
lambdaAvailabilityProvider_lambdaArn = Lens.lens (\LambdaAvailabilityProvider' {lambdaArn} -> lambdaArn) (\s@LambdaAvailabilityProvider' {} a -> s {lambdaArn = a} :: LambdaAvailabilityProvider)

instance Data.FromJSON LambdaAvailabilityProvider where
  parseJSON =
    Data.withObject
      "LambdaAvailabilityProvider"
      ( \x ->
          LambdaAvailabilityProvider'
            Prelude.<$> (x Data..: "LambdaArn")
      )

instance Prelude.Hashable LambdaAvailabilityProvider where
  hashWithSalt _salt LambdaAvailabilityProvider' {..} =
    _salt `Prelude.hashWithSalt` lambdaArn

instance Prelude.NFData LambdaAvailabilityProvider where
  rnf LambdaAvailabilityProvider' {..} =
    Prelude.rnf lambdaArn

instance Data.ToJSON LambdaAvailabilityProvider where
  toJSON LambdaAvailabilityProvider' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("LambdaArn" Data..= lambdaArn)]
      )
