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
-- Module      : Amazonka.FinSpaceData.Types.ErrorInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpaceData.Types.ErrorInfo where

import qualified Amazonka.Core as Core
import Amazonka.FinSpaceData.Types.ErrorCategory
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Error message.
--
-- /See:/ 'newErrorInfo' smart constructor.
data ErrorInfo = ErrorInfo'
  { -- | The category of the error.
    --
    -- -   @VALIDATION@ -The inputs to this request are invalid.
    --
    -- -   @SERVICE_QUOTA_EXCEEDED@ - Service quotas have been exceeded. Please
    --     contact AWS support to increase quotas.
    --
    -- -   @ACCESS_DENIED@ - Missing required permission to perform this
    --     request.
    --
    -- -   @RESOURCE_NOT_FOUND@ - One or more inputs to this request were not
    --     found.
    --
    -- -   @THROTTLING@ - The system temporarily lacks sufficient resources to
    --     process the request.
    --
    -- -   @INTERNAL_SERVICE_EXCEPTION@ - An internal service error has
    --     occurred.
    --
    -- -   @CANCELLED@ - A user recoverable error has occurred.
    errorCategory :: Prelude.Maybe ErrorCategory,
    -- | The text of the error message.
    errorMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ErrorInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCategory', 'errorInfo_errorCategory' - The category of the error.
--
-- -   @VALIDATION@ -The inputs to this request are invalid.
--
-- -   @SERVICE_QUOTA_EXCEEDED@ - Service quotas have been exceeded. Please
--     contact AWS support to increase quotas.
--
-- -   @ACCESS_DENIED@ - Missing required permission to perform this
--     request.
--
-- -   @RESOURCE_NOT_FOUND@ - One or more inputs to this request were not
--     found.
--
-- -   @THROTTLING@ - The system temporarily lacks sufficient resources to
--     process the request.
--
-- -   @INTERNAL_SERVICE_EXCEPTION@ - An internal service error has
--     occurred.
--
-- -   @CANCELLED@ - A user recoverable error has occurred.
--
-- 'errorMessage', 'errorInfo_errorMessage' - The text of the error message.
newErrorInfo ::
  ErrorInfo
newErrorInfo =
  ErrorInfo'
    { errorCategory = Prelude.Nothing,
      errorMessage = Prelude.Nothing
    }

-- | The category of the error.
--
-- -   @VALIDATION@ -The inputs to this request are invalid.
--
-- -   @SERVICE_QUOTA_EXCEEDED@ - Service quotas have been exceeded. Please
--     contact AWS support to increase quotas.
--
-- -   @ACCESS_DENIED@ - Missing required permission to perform this
--     request.
--
-- -   @RESOURCE_NOT_FOUND@ - One or more inputs to this request were not
--     found.
--
-- -   @THROTTLING@ - The system temporarily lacks sufficient resources to
--     process the request.
--
-- -   @INTERNAL_SERVICE_EXCEPTION@ - An internal service error has
--     occurred.
--
-- -   @CANCELLED@ - A user recoverable error has occurred.
errorInfo_errorCategory :: Lens.Lens' ErrorInfo (Prelude.Maybe ErrorCategory)
errorInfo_errorCategory = Lens.lens (\ErrorInfo' {errorCategory} -> errorCategory) (\s@ErrorInfo' {} a -> s {errorCategory = a} :: ErrorInfo)

-- | The text of the error message.
errorInfo_errorMessage :: Lens.Lens' ErrorInfo (Prelude.Maybe Prelude.Text)
errorInfo_errorMessage = Lens.lens (\ErrorInfo' {errorMessage} -> errorMessage) (\s@ErrorInfo' {} a -> s {errorMessage = a} :: ErrorInfo)

instance Core.FromJSON ErrorInfo where
  parseJSON =
    Core.withObject
      "ErrorInfo"
      ( \x ->
          ErrorInfo'
            Prelude.<$> (x Core..:? "errorCategory")
            Prelude.<*> (x Core..:? "errorMessage")
      )

instance Prelude.Hashable ErrorInfo where
  hashWithSalt _salt ErrorInfo' {..} =
    _salt `Prelude.hashWithSalt` errorCategory
      `Prelude.hashWithSalt` errorMessage

instance Prelude.NFData ErrorInfo where
  rnf ErrorInfo' {..} =
    Prelude.rnf errorCategory
      `Prelude.seq` Prelude.rnf errorMessage
