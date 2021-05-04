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
-- Module      : Network.AWS.Greengrass.Types.ErrorDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.ErrorDetail where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details about the error.
--
-- /See:/ 'newErrorDetail' smart constructor.
data ErrorDetail = ErrorDetail'
  { -- | A detailed error message.
    detailedErrorMessage :: Prelude.Maybe Prelude.Text,
    -- | A detailed error code.
    detailedErrorCode :: Prelude.Maybe Prelude.Text
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
-- 'detailedErrorMessage', 'errorDetail_detailedErrorMessage' - A detailed error message.
--
-- 'detailedErrorCode', 'errorDetail_detailedErrorCode' - A detailed error code.
newErrorDetail ::
  ErrorDetail
newErrorDetail =
  ErrorDetail'
    { detailedErrorMessage =
        Prelude.Nothing,
      detailedErrorCode = Prelude.Nothing
    }

-- | A detailed error message.
errorDetail_detailedErrorMessage :: Lens.Lens' ErrorDetail (Prelude.Maybe Prelude.Text)
errorDetail_detailedErrorMessage = Lens.lens (\ErrorDetail' {detailedErrorMessage} -> detailedErrorMessage) (\s@ErrorDetail' {} a -> s {detailedErrorMessage = a} :: ErrorDetail)

-- | A detailed error code.
errorDetail_detailedErrorCode :: Lens.Lens' ErrorDetail (Prelude.Maybe Prelude.Text)
errorDetail_detailedErrorCode = Lens.lens (\ErrorDetail' {detailedErrorCode} -> detailedErrorCode) (\s@ErrorDetail' {} a -> s {detailedErrorCode = a} :: ErrorDetail)

instance Prelude.FromJSON ErrorDetail where
  parseJSON =
    Prelude.withObject
      "ErrorDetail"
      ( \x ->
          ErrorDetail'
            Prelude.<$> (x Prelude..:? "DetailedErrorMessage")
            Prelude.<*> (x Prelude..:? "DetailedErrorCode")
      )

instance Prelude.Hashable ErrorDetail

instance Prelude.NFData ErrorDetail
