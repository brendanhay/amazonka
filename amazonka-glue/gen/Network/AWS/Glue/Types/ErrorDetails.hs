{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object containing error details.
--
-- /See:/ 'newErrorDetails' smart constructor.
data ErrorDetails = ErrorDetails'
  { -- | The error message for an error.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The error code for an error.
    errorCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { errorMessage = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | The error message for an error.
errorDetails_errorMessage :: Lens.Lens' ErrorDetails (Prelude.Maybe Prelude.Text)
errorDetails_errorMessage = Lens.lens (\ErrorDetails' {errorMessage} -> errorMessage) (\s@ErrorDetails' {} a -> s {errorMessage = a} :: ErrorDetails)

-- | The error code for an error.
errorDetails_errorCode :: Lens.Lens' ErrorDetails (Prelude.Maybe Prelude.Text)
errorDetails_errorCode = Lens.lens (\ErrorDetails' {errorCode} -> errorCode) (\s@ErrorDetails' {} a -> s {errorCode = a} :: ErrorDetails)

instance Prelude.FromJSON ErrorDetails where
  parseJSON =
    Prelude.withObject
      "ErrorDetails"
      ( \x ->
          ErrorDetails'
            Prelude.<$> (x Prelude..:? "ErrorMessage")
            Prelude.<*> (x Prelude..:? "ErrorCode")
      )

instance Prelude.Hashable ErrorDetails

instance Prelude.NFData ErrorDetails
