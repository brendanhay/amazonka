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
-- Module      : Amazonka.LakeFormation.Types.ErrorDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.ErrorDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains details about an error.
--
-- /See:/ 'newErrorDetail' smart constructor.
data ErrorDetail = ErrorDetail'
  { -- | The code associated with this error.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | A message describing the error.
    errorMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ErrorDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'errorDetail_errorCode' - The code associated with this error.
--
-- 'errorMessage', 'errorDetail_errorMessage' - A message describing the error.
newErrorDetail ::
  ErrorDetail
newErrorDetail =
  ErrorDetail'
    { errorCode = Prelude.Nothing,
      errorMessage = Prelude.Nothing
    }

-- | The code associated with this error.
errorDetail_errorCode :: Lens.Lens' ErrorDetail (Prelude.Maybe Prelude.Text)
errorDetail_errorCode = Lens.lens (\ErrorDetail' {errorCode} -> errorCode) (\s@ErrorDetail' {} a -> s {errorCode = a} :: ErrorDetail)

-- | A message describing the error.
errorDetail_errorMessage :: Lens.Lens' ErrorDetail (Prelude.Maybe Prelude.Text)
errorDetail_errorMessage = Lens.lens (\ErrorDetail' {errorMessage} -> errorMessage) (\s@ErrorDetail' {} a -> s {errorMessage = a} :: ErrorDetail)

instance Data.FromJSON ErrorDetail where
  parseJSON =
    Data.withObject
      "ErrorDetail"
      ( \x ->
          ErrorDetail'
            Prelude.<$> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "ErrorMessage")
      )

instance Prelude.Hashable ErrorDetail where
  hashWithSalt _salt ErrorDetail' {..} =
    _salt `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage

instance Prelude.NFData ErrorDetail where
  rnf ErrorDetail' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
