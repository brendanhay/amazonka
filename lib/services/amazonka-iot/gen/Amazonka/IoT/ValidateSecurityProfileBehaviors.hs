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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    validateSecurityProfileBehaviorsResponse_validationErrors,
    validateSecurityProfileBehaviorsResponse_valid,
    validateSecurityProfileBehaviorsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoT.Types
import qualified Amazonka.Lens as Lens
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ValidateSecurityProfileBehaviorsResponse'
            Prelude.<$> ( x Core..?> "validationErrors"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "valid")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ValidateSecurityProfileBehaviors

instance
  Prelude.NFData
    ValidateSecurityProfileBehaviors

instance
  Core.ToHeaders
    ValidateSecurityProfileBehaviors
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON ValidateSecurityProfileBehaviors where
  toJSON ValidateSecurityProfileBehaviors' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("behaviors" Core..= behaviors)]
      )

instance Core.ToPath ValidateSecurityProfileBehaviors where
  toPath =
    Prelude.const
      "/security-profile-behaviors/validate"

instance
  Core.ToQuery
    ValidateSecurityProfileBehaviors
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newValidateSecurityProfileBehaviorsResponse' smart constructor.
data ValidateSecurityProfileBehaviorsResponse = ValidateSecurityProfileBehaviorsResponse'
  { -- | The list of any errors found in the behaviors.
    validationErrors :: Prelude.Maybe [ValidationError],
    -- | True if the behaviors were valid.
    valid :: Prelude.Maybe Prelude.Bool,
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
-- 'validationErrors', 'validateSecurityProfileBehaviorsResponse_validationErrors' - The list of any errors found in the behaviors.
--
-- 'valid', 'validateSecurityProfileBehaviorsResponse_valid' - True if the behaviors were valid.
--
-- 'httpStatus', 'validateSecurityProfileBehaviorsResponse_httpStatus' - The response's http status code.
newValidateSecurityProfileBehaviorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ValidateSecurityProfileBehaviorsResponse
newValidateSecurityProfileBehaviorsResponse
  pHttpStatus_ =
    ValidateSecurityProfileBehaviorsResponse'
      { validationErrors =
          Prelude.Nothing,
        valid = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The list of any errors found in the behaviors.
validateSecurityProfileBehaviorsResponse_validationErrors :: Lens.Lens' ValidateSecurityProfileBehaviorsResponse (Prelude.Maybe [ValidationError])
validateSecurityProfileBehaviorsResponse_validationErrors = Lens.lens (\ValidateSecurityProfileBehaviorsResponse' {validationErrors} -> validationErrors) (\s@ValidateSecurityProfileBehaviorsResponse' {} a -> s {validationErrors = a} :: ValidateSecurityProfileBehaviorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | True if the behaviors were valid.
validateSecurityProfileBehaviorsResponse_valid :: Lens.Lens' ValidateSecurityProfileBehaviorsResponse (Prelude.Maybe Prelude.Bool)
validateSecurityProfileBehaviorsResponse_valid = Lens.lens (\ValidateSecurityProfileBehaviorsResponse' {valid} -> valid) (\s@ValidateSecurityProfileBehaviorsResponse' {} a -> s {valid = a} :: ValidateSecurityProfileBehaviorsResponse)

-- | The response's http status code.
validateSecurityProfileBehaviorsResponse_httpStatus :: Lens.Lens' ValidateSecurityProfileBehaviorsResponse Prelude.Int
validateSecurityProfileBehaviorsResponse_httpStatus = Lens.lens (\ValidateSecurityProfileBehaviorsResponse' {httpStatus} -> httpStatus) (\s@ValidateSecurityProfileBehaviorsResponse' {} a -> s {httpStatus = a} :: ValidateSecurityProfileBehaviorsResponse)

instance
  Prelude.NFData
    ValidateSecurityProfileBehaviorsResponse
