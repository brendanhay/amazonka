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
-- Module      : Network.AWS.Glue.Types.ErrorDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ErrorDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An object containing error details.
--
-- /See:/ 'newErrorDetails' smart constructor.
data ErrorDetails = ErrorDetails'
  { -- | The error message for an error.
    errorMessage :: Core.Maybe Core.Text,
    -- | The error code for an error.
    errorCode :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ErrorDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorMessage', 'errorDetails_errorMessage' - The error message for an error.
--
-- 'errorCode', 'errorDetails_errorCode' - The error code for an error.
newErrorDetails ::
  ErrorDetails
newErrorDetails =
  ErrorDetails'
    { errorMessage = Core.Nothing,
      errorCode = Core.Nothing
    }

-- | The error message for an error.
errorDetails_errorMessage :: Lens.Lens' ErrorDetails (Core.Maybe Core.Text)
errorDetails_errorMessage = Lens.lens (\ErrorDetails' {errorMessage} -> errorMessage) (\s@ErrorDetails' {} a -> s {errorMessage = a} :: ErrorDetails)

-- | The error code for an error.
errorDetails_errorCode :: Lens.Lens' ErrorDetails (Core.Maybe Core.Text)
errorDetails_errorCode = Lens.lens (\ErrorDetails' {errorCode} -> errorCode) (\s@ErrorDetails' {} a -> s {errorCode = a} :: ErrorDetails)

instance Core.FromJSON ErrorDetails where
  parseJSON =
    Core.withObject
      "ErrorDetails"
      ( \x ->
          ErrorDetails'
            Core.<$> (x Core..:? "ErrorMessage")
            Core.<*> (x Core..:? "ErrorCode")
      )

instance Core.Hashable ErrorDetails

instance Core.NFData ErrorDetails
