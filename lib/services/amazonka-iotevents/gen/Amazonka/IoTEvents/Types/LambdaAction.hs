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
-- Module      : Amazonka.IoTEvents.Types.LambdaAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.LambdaAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTEvents.Types.Payload
import qualified Amazonka.Prelude as Prelude

-- | Calls a Lambda function, passing in information about the detector model
-- instance and the event that triggered the action.
--
-- /See:/ 'newLambdaAction' smart constructor.
data LambdaAction = LambdaAction'
  { -- | You can configure the action payload when you send a message to a Lambda
    -- function.
    payload :: Prelude.Maybe Payload,
    -- | The ARN of the Lambda function that is executed.
    functionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'payload', 'lambdaAction_payload' - You can configure the action payload when you send a message to a Lambda
-- function.
--
-- 'functionArn', 'lambdaAction_functionArn' - The ARN of the Lambda function that is executed.
newLambdaAction ::
  -- | 'functionArn'
  Prelude.Text ->
  LambdaAction
newLambdaAction pFunctionArn_ =
  LambdaAction'
    { payload = Prelude.Nothing,
      functionArn = pFunctionArn_
    }

-- | You can configure the action payload when you send a message to a Lambda
-- function.
lambdaAction_payload :: Lens.Lens' LambdaAction (Prelude.Maybe Payload)
lambdaAction_payload = Lens.lens (\LambdaAction' {payload} -> payload) (\s@LambdaAction' {} a -> s {payload = a} :: LambdaAction)

-- | The ARN of the Lambda function that is executed.
lambdaAction_functionArn :: Lens.Lens' LambdaAction Prelude.Text
lambdaAction_functionArn = Lens.lens (\LambdaAction' {functionArn} -> functionArn) (\s@LambdaAction' {} a -> s {functionArn = a} :: LambdaAction)

instance Core.FromJSON LambdaAction where
  parseJSON =
    Core.withObject
      "LambdaAction"
      ( \x ->
          LambdaAction'
            Prelude.<$> (x Core..:? "payload")
            Prelude.<*> (x Core..: "functionArn")
      )

instance Prelude.Hashable LambdaAction where
  hashWithSalt _salt LambdaAction' {..} =
    _salt `Prelude.hashWithSalt` payload
      `Prelude.hashWithSalt` functionArn

instance Prelude.NFData LambdaAction where
  rnf LambdaAction' {..} =
    Prelude.rnf payload
      `Prelude.seq` Prelude.rnf functionArn

instance Core.ToJSON LambdaAction where
  toJSON LambdaAction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("payload" Core..=) Prelude.<$> payload,
            Prelude.Just ("functionArn" Core..= functionArn)
          ]
      )
