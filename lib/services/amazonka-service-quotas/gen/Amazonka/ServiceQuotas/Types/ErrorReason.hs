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
-- Module      : Amazonka.ServiceQuotas.Types.ErrorReason
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceQuotas.Types.ErrorReason where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceQuotas.Types.ErrorCode

-- | An error that explains why an action did not succeed.
--
-- /See:/ 'newErrorReason' smart constructor.
data ErrorReason = ErrorReason'
  { -- | Service Quotas returns the following error values:
    --
    -- -   @DEPENDENCY_ACCESS_DENIED_ERROR@ - The caller does not have the
    --     required permissions to complete the action. To resolve the error,
    --     you must have permission to access the service or quota.
    --
    -- -   @DEPENDENCY_THROTTLING_ERROR@ - The service is throttling Service
    --     Quotas.
    --
    -- -   @DEPENDENCY_SERVICE_ERROR@ - The service is not available.
    --
    -- -   @SERVICE_QUOTA_NOT_AVAILABLE_ERROR@ - There was an error in Service
    --     Quotas.
    errorCode :: Prelude.Maybe ErrorCode,
    -- | The error message.
    errorMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ErrorReason' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'errorReason_errorCode' - Service Quotas returns the following error values:
--
-- -   @DEPENDENCY_ACCESS_DENIED_ERROR@ - The caller does not have the
--     required permissions to complete the action. To resolve the error,
--     you must have permission to access the service or quota.
--
-- -   @DEPENDENCY_THROTTLING_ERROR@ - The service is throttling Service
--     Quotas.
--
-- -   @DEPENDENCY_SERVICE_ERROR@ - The service is not available.
--
-- -   @SERVICE_QUOTA_NOT_AVAILABLE_ERROR@ - There was an error in Service
--     Quotas.
--
-- 'errorMessage', 'errorReason_errorMessage' - The error message.
newErrorReason ::
  ErrorReason
newErrorReason =
  ErrorReason'
    { errorCode = Prelude.Nothing,
      errorMessage = Prelude.Nothing
    }

-- | Service Quotas returns the following error values:
--
-- -   @DEPENDENCY_ACCESS_DENIED_ERROR@ - The caller does not have the
--     required permissions to complete the action. To resolve the error,
--     you must have permission to access the service or quota.
--
-- -   @DEPENDENCY_THROTTLING_ERROR@ - The service is throttling Service
--     Quotas.
--
-- -   @DEPENDENCY_SERVICE_ERROR@ - The service is not available.
--
-- -   @SERVICE_QUOTA_NOT_AVAILABLE_ERROR@ - There was an error in Service
--     Quotas.
errorReason_errorCode :: Lens.Lens' ErrorReason (Prelude.Maybe ErrorCode)
errorReason_errorCode = Lens.lens (\ErrorReason' {errorCode} -> errorCode) (\s@ErrorReason' {} a -> s {errorCode = a} :: ErrorReason)

-- | The error message.
errorReason_errorMessage :: Lens.Lens' ErrorReason (Prelude.Maybe Prelude.Text)
errorReason_errorMessage = Lens.lens (\ErrorReason' {errorMessage} -> errorMessage) (\s@ErrorReason' {} a -> s {errorMessage = a} :: ErrorReason)

instance Data.FromJSON ErrorReason where
  parseJSON =
    Data.withObject
      "ErrorReason"
      ( \x ->
          ErrorReason'
            Prelude.<$> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "ErrorMessage")
      )

instance Prelude.Hashable ErrorReason where
  hashWithSalt _salt ErrorReason' {..} =
    _salt
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage

instance Prelude.NFData ErrorReason where
  rnf ErrorReason' {..} =
    Prelude.rnf errorCode `Prelude.seq`
      Prelude.rnf errorMessage
