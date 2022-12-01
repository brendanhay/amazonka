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
-- Module      : Amazonka.IoT.PutVerificationStateOnViolation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Set a verification state and provide a description of that verification
-- state on a violation (detect alarm).
module Amazonka.IoT.PutVerificationStateOnViolation
  ( -- * Creating a Request
    PutVerificationStateOnViolation (..),
    newPutVerificationStateOnViolation,

    -- * Request Lenses
    putVerificationStateOnViolation_verificationStateDescription,
    putVerificationStateOnViolation_violationId,
    putVerificationStateOnViolation_verificationState,

    -- * Destructuring the Response
    PutVerificationStateOnViolationResponse (..),
    newPutVerificationStateOnViolationResponse,

    -- * Response Lenses
    putVerificationStateOnViolationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutVerificationStateOnViolation' smart constructor.
data PutVerificationStateOnViolation = PutVerificationStateOnViolation'
  { -- | The description of the verification state of the violation (detect
    -- alarm).
    verificationStateDescription :: Prelude.Maybe Prelude.Text,
    -- | The violation ID.
    violationId :: Prelude.Text,
    -- | The verification state of the violation.
    verificationState :: VerificationState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutVerificationStateOnViolation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'verificationStateDescription', 'putVerificationStateOnViolation_verificationStateDescription' - The description of the verification state of the violation (detect
-- alarm).
--
-- 'violationId', 'putVerificationStateOnViolation_violationId' - The violation ID.
--
-- 'verificationState', 'putVerificationStateOnViolation_verificationState' - The verification state of the violation.
newPutVerificationStateOnViolation ::
  -- | 'violationId'
  Prelude.Text ->
  -- | 'verificationState'
  VerificationState ->
  PutVerificationStateOnViolation
newPutVerificationStateOnViolation
  pViolationId_
  pVerificationState_ =
    PutVerificationStateOnViolation'
      { verificationStateDescription =
          Prelude.Nothing,
        violationId = pViolationId_,
        verificationState = pVerificationState_
      }

-- | The description of the verification state of the violation (detect
-- alarm).
putVerificationStateOnViolation_verificationStateDescription :: Lens.Lens' PutVerificationStateOnViolation (Prelude.Maybe Prelude.Text)
putVerificationStateOnViolation_verificationStateDescription = Lens.lens (\PutVerificationStateOnViolation' {verificationStateDescription} -> verificationStateDescription) (\s@PutVerificationStateOnViolation' {} a -> s {verificationStateDescription = a} :: PutVerificationStateOnViolation)

-- | The violation ID.
putVerificationStateOnViolation_violationId :: Lens.Lens' PutVerificationStateOnViolation Prelude.Text
putVerificationStateOnViolation_violationId = Lens.lens (\PutVerificationStateOnViolation' {violationId} -> violationId) (\s@PutVerificationStateOnViolation' {} a -> s {violationId = a} :: PutVerificationStateOnViolation)

-- | The verification state of the violation.
putVerificationStateOnViolation_verificationState :: Lens.Lens' PutVerificationStateOnViolation VerificationState
putVerificationStateOnViolation_verificationState = Lens.lens (\PutVerificationStateOnViolation' {verificationState} -> verificationState) (\s@PutVerificationStateOnViolation' {} a -> s {verificationState = a} :: PutVerificationStateOnViolation)

instance
  Core.AWSRequest
    PutVerificationStateOnViolation
  where
  type
    AWSResponse PutVerificationStateOnViolation =
      PutVerificationStateOnViolationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutVerificationStateOnViolationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutVerificationStateOnViolation
  where
  hashWithSalt
    _salt
    PutVerificationStateOnViolation' {..} =
      _salt
        `Prelude.hashWithSalt` verificationStateDescription
        `Prelude.hashWithSalt` violationId
        `Prelude.hashWithSalt` verificationState

instance
  Prelude.NFData
    PutVerificationStateOnViolation
  where
  rnf PutVerificationStateOnViolation' {..} =
    Prelude.rnf verificationStateDescription
      `Prelude.seq` Prelude.rnf violationId
      `Prelude.seq` Prelude.rnf verificationState

instance
  Core.ToHeaders
    PutVerificationStateOnViolation
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON PutVerificationStateOnViolation where
  toJSON PutVerificationStateOnViolation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("verificationStateDescription" Core..=)
              Prelude.<$> verificationStateDescription,
            Prelude.Just
              ("verificationState" Core..= verificationState)
          ]
      )

instance Core.ToPath PutVerificationStateOnViolation where
  toPath PutVerificationStateOnViolation' {..} =
    Prelude.mconcat
      [ "/violations/verification-state/",
        Core.toBS violationId
      ]

instance Core.ToQuery PutVerificationStateOnViolation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutVerificationStateOnViolationResponse' smart constructor.
data PutVerificationStateOnViolationResponse = PutVerificationStateOnViolationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutVerificationStateOnViolationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putVerificationStateOnViolationResponse_httpStatus' - The response's http status code.
newPutVerificationStateOnViolationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutVerificationStateOnViolationResponse
newPutVerificationStateOnViolationResponse
  pHttpStatus_ =
    PutVerificationStateOnViolationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
putVerificationStateOnViolationResponse_httpStatus :: Lens.Lens' PutVerificationStateOnViolationResponse Prelude.Int
putVerificationStateOnViolationResponse_httpStatus = Lens.lens (\PutVerificationStateOnViolationResponse' {httpStatus} -> httpStatus) (\s@PutVerificationStateOnViolationResponse' {} a -> s {httpStatus = a} :: PutVerificationStateOnViolationResponse)

instance
  Prelude.NFData
    PutVerificationStateOnViolationResponse
  where
  rnf PutVerificationStateOnViolationResponse' {..} =
    Prelude.rnf httpStatus
