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
-- Module      : Amazonka.FinSpace.Types.ErrorInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpace.Types.ErrorInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpace.Types.ErrorDetails
import qualified Amazonka.Prelude as Prelude

-- | Provides details in the event of a failed flow, including the error type
-- and the related error message.
--
-- /See:/ 'newErrorInfo' smart constructor.
data ErrorInfo = ErrorInfo'
  { -- | Specifies the error message that appears if a flow fails.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | Specifies the type of error.
    errorType :: Prelude.Maybe ErrorDetails
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
-- 'errorMessage', 'errorInfo_errorMessage' - Specifies the error message that appears if a flow fails.
--
-- 'errorType', 'errorInfo_errorType' - Specifies the type of error.
newErrorInfo ::
  ErrorInfo
newErrorInfo =
  ErrorInfo'
    { errorMessage = Prelude.Nothing,
      errorType = Prelude.Nothing
    }

-- | Specifies the error message that appears if a flow fails.
errorInfo_errorMessage :: Lens.Lens' ErrorInfo (Prelude.Maybe Prelude.Text)
errorInfo_errorMessage = Lens.lens (\ErrorInfo' {errorMessage} -> errorMessage) (\s@ErrorInfo' {} a -> s {errorMessage = a} :: ErrorInfo)

-- | Specifies the type of error.
errorInfo_errorType :: Lens.Lens' ErrorInfo (Prelude.Maybe ErrorDetails)
errorInfo_errorType = Lens.lens (\ErrorInfo' {errorType} -> errorType) (\s@ErrorInfo' {} a -> s {errorType = a} :: ErrorInfo)

instance Data.FromJSON ErrorInfo where
  parseJSON =
    Data.withObject
      "ErrorInfo"
      ( \x ->
          ErrorInfo'
            Prelude.<$> (x Data..:? "errorMessage")
            Prelude.<*> (x Data..:? "errorType")
      )

instance Prelude.Hashable ErrorInfo where
  hashWithSalt _salt ErrorInfo' {..} =
    _salt
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` errorType

instance Prelude.NFData ErrorInfo where
  rnf ErrorInfo' {..} =
    Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf errorType
