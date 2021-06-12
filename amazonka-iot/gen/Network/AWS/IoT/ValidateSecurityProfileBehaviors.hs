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
-- Module      : Network.AWS.IoT.ValidateSecurityProfileBehaviors
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Validates a Device Defender security profile behaviors specification.
module Network.AWS.IoT.ValidateSecurityProfileBehaviors
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

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newValidateSecurityProfileBehaviors' smart constructor.
data ValidateSecurityProfileBehaviors = ValidateSecurityProfileBehaviors'
  { -- | Specifies the behaviors that, when violated by a device (thing), cause
    -- an alert.
    behaviors :: [Behavior]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.mempty
    }

-- | Specifies the behaviors that, when violated by a device (thing), cause
-- an alert.
validateSecurityProfileBehaviors_behaviors :: Lens.Lens' ValidateSecurityProfileBehaviors [Behavior]
validateSecurityProfileBehaviors_behaviors = Lens.lens (\ValidateSecurityProfileBehaviors' {behaviors} -> behaviors) (\s@ValidateSecurityProfileBehaviors' {} a -> s {behaviors = a} :: ValidateSecurityProfileBehaviors) Core.. Lens._Coerce

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
            Core.<$> (x Core..?> "validationErrors" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "valid")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    ValidateSecurityProfileBehaviors

instance Core.NFData ValidateSecurityProfileBehaviors

instance
  Core.ToHeaders
    ValidateSecurityProfileBehaviors
  where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON ValidateSecurityProfileBehaviors where
  toJSON ValidateSecurityProfileBehaviors' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("behaviors" Core..= behaviors)]
      )

instance Core.ToPath ValidateSecurityProfileBehaviors where
  toPath =
    Core.const "/security-profile-behaviors/validate"

instance
  Core.ToQuery
    ValidateSecurityProfileBehaviors
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newValidateSecurityProfileBehaviorsResponse' smart constructor.
data ValidateSecurityProfileBehaviorsResponse = ValidateSecurityProfileBehaviorsResponse'
  { -- | The list of any errors found in the behaviors.
    validationErrors :: Core.Maybe [ValidationError],
    -- | True if the behaviors were valid.
    valid :: Core.Maybe Core.Bool,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ValidateSecurityProfileBehaviorsResponse
newValidateSecurityProfileBehaviorsResponse
  pHttpStatus_ =
    ValidateSecurityProfileBehaviorsResponse'
      { validationErrors =
          Core.Nothing,
        valid = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The list of any errors found in the behaviors.
validateSecurityProfileBehaviorsResponse_validationErrors :: Lens.Lens' ValidateSecurityProfileBehaviorsResponse (Core.Maybe [ValidationError])
validateSecurityProfileBehaviorsResponse_validationErrors = Lens.lens (\ValidateSecurityProfileBehaviorsResponse' {validationErrors} -> validationErrors) (\s@ValidateSecurityProfileBehaviorsResponse' {} a -> s {validationErrors = a} :: ValidateSecurityProfileBehaviorsResponse) Core.. Lens.mapping Lens._Coerce

-- | True if the behaviors were valid.
validateSecurityProfileBehaviorsResponse_valid :: Lens.Lens' ValidateSecurityProfileBehaviorsResponse (Core.Maybe Core.Bool)
validateSecurityProfileBehaviorsResponse_valid = Lens.lens (\ValidateSecurityProfileBehaviorsResponse' {valid} -> valid) (\s@ValidateSecurityProfileBehaviorsResponse' {} a -> s {valid = a} :: ValidateSecurityProfileBehaviorsResponse)

-- | The response's http status code.
validateSecurityProfileBehaviorsResponse_httpStatus :: Lens.Lens' ValidateSecurityProfileBehaviorsResponse Core.Int
validateSecurityProfileBehaviorsResponse_httpStatus = Lens.lens (\ValidateSecurityProfileBehaviorsResponse' {httpStatus} -> httpStatus) (\s@ValidateSecurityProfileBehaviorsResponse' {} a -> s {httpStatus = a} :: ValidateSecurityProfileBehaviorsResponse)

instance
  Core.NFData
    ValidateSecurityProfileBehaviorsResponse
