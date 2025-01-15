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
-- Module      : Amazonka.Greengrass.Types.ErrorDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.ErrorDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about the error.
--
-- /See:/ 'newErrorDetail' smart constructor.
data ErrorDetail = ErrorDetail'
  { -- | A detailed error code.
    detailedErrorCode :: Prelude.Maybe Prelude.Text,
    -- | A detailed error message.
    detailedErrorMessage :: Prelude.Maybe Prelude.Text
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
-- 'detailedErrorCode', 'errorDetail_detailedErrorCode' - A detailed error code.
--
-- 'detailedErrorMessage', 'errorDetail_detailedErrorMessage' - A detailed error message.
newErrorDetail ::
  ErrorDetail
newErrorDetail =
  ErrorDetail'
    { detailedErrorCode = Prelude.Nothing,
      detailedErrorMessage = Prelude.Nothing
    }

-- | A detailed error code.
errorDetail_detailedErrorCode :: Lens.Lens' ErrorDetail (Prelude.Maybe Prelude.Text)
errorDetail_detailedErrorCode = Lens.lens (\ErrorDetail' {detailedErrorCode} -> detailedErrorCode) (\s@ErrorDetail' {} a -> s {detailedErrorCode = a} :: ErrorDetail)

-- | A detailed error message.
errorDetail_detailedErrorMessage :: Lens.Lens' ErrorDetail (Prelude.Maybe Prelude.Text)
errorDetail_detailedErrorMessage = Lens.lens (\ErrorDetail' {detailedErrorMessage} -> detailedErrorMessage) (\s@ErrorDetail' {} a -> s {detailedErrorMessage = a} :: ErrorDetail)

instance Data.FromJSON ErrorDetail where
  parseJSON =
    Data.withObject
      "ErrorDetail"
      ( \x ->
          ErrorDetail'
            Prelude.<$> (x Data..:? "DetailedErrorCode")
            Prelude.<*> (x Data..:? "DetailedErrorMessage")
      )

instance Prelude.Hashable ErrorDetail where
  hashWithSalt _salt ErrorDetail' {..} =
    _salt
      `Prelude.hashWithSalt` detailedErrorCode
      `Prelude.hashWithSalt` detailedErrorMessage

instance Prelude.NFData ErrorDetail where
  rnf ErrorDetail' {..} =
    Prelude.rnf detailedErrorCode `Prelude.seq`
      Prelude.rnf detailedErrorMessage
