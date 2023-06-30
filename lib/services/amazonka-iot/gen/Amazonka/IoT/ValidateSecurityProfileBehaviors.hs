{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoT.ValidateSecurityProfileBehaviors
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Validates a Device Defender security profile behaviors specification.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ValidateSecurityProfileBehaviors>
-- action.
module Amazonka.IoT.ValidateSecurityProfileBehaviors
  ( -- * Creating a Request
    ValidateSecurityProfileBehaviors (..),
    newValidateSecurityProfileBehaviors,

    -- * Request Lenses
    validateSecurityProfileBehaviors_behaviors,

    -- * Destructuring the Response
    ValidateSecurityProfileBehaviorsResponse (..),
    newValidateSecurityProfileBehaviorsResponse,

    -- * Response Lenses
    validateSecurityProfileBehaviorsResponse_valid,
    validateSecurityProfileBehaviorsResponse_validationErrors,
    validateSecurityProfileBehaviorsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newValidateSecurityProfileBehaviors' smart constructor.
data ValidateSecurityProfileBehaviors = ValidateSecurityProfileBehaviors'
  { -- | Specifies the behaviors that, when violated by a device (thing), cause
    -- an alert.
    behaviors :: [Behavior]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValidateSecurityProfileBehaviors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'behaviors', 'validateSecurityProfileBehaviors_behaviors' - Specifies the behaviors that, when violated by a device (thing), cause
-- an alert.
newValidateSecurityProfileBehaviors ::
  ValidateSecurityProfileBehaviors
newValidateSecurityProfileBehaviors =
  ValidateSecurityProfileBehaviors'
    { behaviors =
        Prelude.mempty
    }

-- | Specifies the behaviors that, when violated by a device (thing), cause
-- an alert.
validateSecurityProfileBehaviors_behaviors :: Lens.Lens' ValidateSecurityProfileBehaviors [Behavior]
validateSecurityProfileBehaviors_behaviors = Lens.lens (\ValidateSecurityProfileBehaviors' {behaviors} -> behaviors) (\s@ValidateSecurityProfileBehaviors' {} a -> s {behaviors = a} :: ValidateSecurityProfileBehaviors) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    ValidateSecurityProfileBehaviors
  where
  type
    AWSResponse ValidateSecurityProfileBehaviors =
      ValidateSecurityProfileBehaviorsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ValidateSecurityProfileBehaviorsResponse'
            Prelude.<$> (x Data..?> "valid")
            Prelude.<*> ( x
                            Data..?> "validationErrors"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ValidateSecurityProfileBehaviors
  where
  hashWithSalt
    _salt
    ValidateSecurityProfileBehaviors' {..} =
      _salt `Prelude.hashWithSalt` behaviors

instance
  Prelude.NFData
    ValidateSecurityProfileBehaviors
  where
  rnf ValidateSecurityProfileBehaviors' {..} =
    Prelude.rnf behaviors

instance
  Data.ToHeaders
    ValidateSecurityProfileBehaviors
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON ValidateSecurityProfileBehaviors where
  toJSON ValidateSecurityProfileBehaviors' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("behaviors" Data..= behaviors)]
      )

instance Data.ToPath ValidateSecurityProfileBehaviors where
  toPath =
    Prelude.const
      "/security-profile-behaviors/validate"

instance
  Data.ToQuery
    ValidateSecurityProfileBehaviors
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newValidateSecurityProfileBehaviorsResponse' smart constructor.
data ValidateSecurityProfileBehaviorsResponse = ValidateSecurityProfileBehaviorsResponse'
  { -- | True if the behaviors were valid.
    valid :: Prelude.Maybe Prelude.Bool,
    -- | The list of any errors found in the behaviors.
    validationErrors :: Prelude.Maybe [ValidationError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValidateSecurityProfileBehaviorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'valid', 'validateSecurityProfileBehaviorsResponse_valid' - True if the behaviors were valid.
--
-- 'validationErrors', 'validateSecurityProfileBehaviorsResponse_validationErrors' - The list of any errors found in the behaviors.
--
-- 'httpStatus', 'validateSecurityProfileBehaviorsResponse_httpStatus' - The response's http status code.
newValidateSecurityProfileBehaviorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ValidateSecurityProfileBehaviorsResponse
newValidateSecurityProfileBehaviorsResponse
  pHttpStatus_ =
    ValidateSecurityProfileBehaviorsResponse'
      { valid =
          Prelude.Nothing,
        validationErrors =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | True if the behaviors were valid.
validateSecurityProfileBehaviorsResponse_valid :: Lens.Lens' ValidateSecurityProfileBehaviorsResponse (Prelude.Maybe Prelude.Bool)
validateSecurityProfileBehaviorsResponse_valid = Lens.lens (\ValidateSecurityProfileBehaviorsResponse' {valid} -> valid) (\s@ValidateSecurityProfileBehaviorsResponse' {} a -> s {valid = a} :: ValidateSecurityProfileBehaviorsResponse)

-- | The list of any errors found in the behaviors.
validateSecurityProfileBehaviorsResponse_validationErrors :: Lens.Lens' ValidateSecurityProfileBehaviorsResponse (Prelude.Maybe [ValidationError])
validateSecurityProfileBehaviorsResponse_validationErrors = Lens.lens (\ValidateSecurityProfileBehaviorsResponse' {validationErrors} -> validationErrors) (\s@ValidateSecurityProfileBehaviorsResponse' {} a -> s {validationErrors = a} :: ValidateSecurityProfileBehaviorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
validateSecurityProfileBehaviorsResponse_httpStatus :: Lens.Lens' ValidateSecurityProfileBehaviorsResponse Prelude.Int
validateSecurityProfileBehaviorsResponse_httpStatus = Lens.lens (\ValidateSecurityProfileBehaviorsResponse' {httpStatus} -> httpStatus) (\s@ValidateSecurityProfileBehaviorsResponse' {} a -> s {httpStatus = a} :: ValidateSecurityProfileBehaviorsResponse)

instance
  Prelude.NFData
    ValidateSecurityProfileBehaviorsResponse
  where
  rnf ValidateSecurityProfileBehaviorsResponse' {..} =
    Prelude.rnf valid
      `Prelude.seq` Prelude.rnf validationErrors
      `Prelude.seq` Prelude.rnf httpStatus
