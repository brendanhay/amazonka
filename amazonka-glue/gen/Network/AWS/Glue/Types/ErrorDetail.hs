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
-- Module      : Network.AWS.Glue.Types.ErrorDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ErrorDetail where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains details about an error.
--
-- /See:/ 'newErrorDetail' smart constructor.
data ErrorDetail = ErrorDetail'
  { -- | A message describing the error.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The code associated with this error.
    errorCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ErrorDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorMessage', 'errorDetail_errorMessage' - A message describing the error.
--
-- 'errorCode', 'errorDetail_errorCode' - The code associated with this error.
newErrorDetail ::
  ErrorDetail
newErrorDetail =
  ErrorDetail'
    { errorMessage = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | A message describing the error.
errorDetail_errorMessage :: Lens.Lens' ErrorDetail (Prelude.Maybe Prelude.Text)
errorDetail_errorMessage = Lens.lens (\ErrorDetail' {errorMessage} -> errorMessage) (\s@ErrorDetail' {} a -> s {errorMessage = a} :: ErrorDetail)

-- | The code associated with this error.
errorDetail_errorCode :: Lens.Lens' ErrorDetail (Prelude.Maybe Prelude.Text)
errorDetail_errorCode = Lens.lens (\ErrorDetail' {errorCode} -> errorCode) (\s@ErrorDetail' {} a -> s {errorCode = a} :: ErrorDetail)

instance Prelude.FromJSON ErrorDetail where
  parseJSON =
    Prelude.withObject
      "ErrorDetail"
      ( \x ->
          ErrorDetail'
            Prelude.<$> (x Prelude..:? "ErrorMessage")
            Prelude.<*> (x Prelude..:? "ErrorCode")
      )

instance Prelude.Hashable ErrorDetail

instance Prelude.NFData ErrorDetail
