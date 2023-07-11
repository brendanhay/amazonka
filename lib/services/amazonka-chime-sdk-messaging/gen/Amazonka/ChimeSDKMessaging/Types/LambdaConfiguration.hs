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
-- Module      : Amazonka.ChimeSDKMessaging.Types.LambdaConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKMessaging.Types.LambdaConfiguration where

import Amazonka.ChimeSDKMessaging.Types.InvocationType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Stores metadata about a Lambda processor.
--
-- /See:/ 'newLambdaConfiguration' smart constructor.
data LambdaConfiguration = LambdaConfiguration'
  { -- | The ARN of the Lambda message processing function.
    resourceArn :: Prelude.Text,
    -- | Controls how the Lambda function is invoked.
    invocationType :: InvocationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'lambdaConfiguration_resourceArn' - The ARN of the Lambda message processing function.
--
-- 'invocationType', 'lambdaConfiguration_invocationType' - Controls how the Lambda function is invoked.
newLambdaConfiguration ::
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'invocationType'
  InvocationType ->
  LambdaConfiguration
newLambdaConfiguration pResourceArn_ pInvocationType_ =
  LambdaConfiguration'
    { resourceArn = pResourceArn_,
      invocationType = pInvocationType_
    }

-- | The ARN of the Lambda message processing function.
lambdaConfiguration_resourceArn :: Lens.Lens' LambdaConfiguration Prelude.Text
lambdaConfiguration_resourceArn = Lens.lens (\LambdaConfiguration' {resourceArn} -> resourceArn) (\s@LambdaConfiguration' {} a -> s {resourceArn = a} :: LambdaConfiguration)

-- | Controls how the Lambda function is invoked.
lambdaConfiguration_invocationType :: Lens.Lens' LambdaConfiguration InvocationType
lambdaConfiguration_invocationType = Lens.lens (\LambdaConfiguration' {invocationType} -> invocationType) (\s@LambdaConfiguration' {} a -> s {invocationType = a} :: LambdaConfiguration)

instance Data.FromJSON LambdaConfiguration where
  parseJSON =
    Data.withObject
      "LambdaConfiguration"
      ( \x ->
          LambdaConfiguration'
            Prelude.<$> (x Data..: "ResourceArn")
            Prelude.<*> (x Data..: "InvocationType")
      )

instance Prelude.Hashable LambdaConfiguration where
  hashWithSalt _salt LambdaConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` invocationType

instance Prelude.NFData LambdaConfiguration where
  rnf LambdaConfiguration' {..} =
    Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf invocationType

instance Data.ToJSON LambdaConfiguration where
  toJSON LambdaConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceArn" Data..= resourceArn),
            Prelude.Just
              ("InvocationType" Data..= invocationType)
          ]
      )
