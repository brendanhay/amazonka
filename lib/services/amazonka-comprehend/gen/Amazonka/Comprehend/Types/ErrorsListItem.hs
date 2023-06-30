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
-- Module      : Amazonka.Comprehend.Types.ErrorsListItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.ErrorsListItem where

import Amazonka.Comprehend.Types.PageBasedErrorCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Text extraction encountered one or more page-level errors in the input
-- document.
--
-- The @ErrorCode@ contains one of the following values:
--
-- -   TEXTRACT_BAD_PAGE - Amazon Textract cannot read the page. For more
--     information about page limits in Amazon Textract, see
--     <https://docs.aws.amazon.com/textract/latest/dg/limits-document.html Page Quotas in Amazon Textract>.
--
-- -   TEXTRACT_PROVISIONED_THROUGHPUT_EXCEEDED - The number of requests
--     exceeded your throughput limit. For more information about
--     throughput quotas in Amazon Textract, see
--     <https://docs.aws.amazon.com/textract/latest/dg/limits-quotas-explained.html Default quotas in Amazon Textract>.
--
-- -   PAGE_CHARACTERS_EXCEEDED - Too many text characters on the page
--     (10,000 characters maximum).
--
-- -   PAGE_SIZE_EXCEEDED - The maximum page size is 10 MB.
--
-- -   INTERNAL_SERVER_ERROR - The request encountered a service issue. Try
--     the API request again.
--
-- /See:/ 'newErrorsListItem' smart constructor.
data ErrorsListItem = ErrorsListItem'
  { -- | Error code for the cause of the error.
    errorCode :: Prelude.Maybe PageBasedErrorCode,
    -- | Text message explaining the reason for the error.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | Page number where the error occurred.
    page :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ErrorsListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'errorsListItem_errorCode' - Error code for the cause of the error.
--
-- 'errorMessage', 'errorsListItem_errorMessage' - Text message explaining the reason for the error.
--
-- 'page', 'errorsListItem_page' - Page number where the error occurred.
newErrorsListItem ::
  ErrorsListItem
newErrorsListItem =
  ErrorsListItem'
    { errorCode = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      page = Prelude.Nothing
    }

-- | Error code for the cause of the error.
errorsListItem_errorCode :: Lens.Lens' ErrorsListItem (Prelude.Maybe PageBasedErrorCode)
errorsListItem_errorCode = Lens.lens (\ErrorsListItem' {errorCode} -> errorCode) (\s@ErrorsListItem' {} a -> s {errorCode = a} :: ErrorsListItem)

-- | Text message explaining the reason for the error.
errorsListItem_errorMessage :: Lens.Lens' ErrorsListItem (Prelude.Maybe Prelude.Text)
errorsListItem_errorMessage = Lens.lens (\ErrorsListItem' {errorMessage} -> errorMessage) (\s@ErrorsListItem' {} a -> s {errorMessage = a} :: ErrorsListItem)

-- | Page number where the error occurred.
errorsListItem_page :: Lens.Lens' ErrorsListItem (Prelude.Maybe Prelude.Int)
errorsListItem_page = Lens.lens (\ErrorsListItem' {page} -> page) (\s@ErrorsListItem' {} a -> s {page = a} :: ErrorsListItem)

instance Data.FromJSON ErrorsListItem where
  parseJSON =
    Data.withObject
      "ErrorsListItem"
      ( \x ->
          ErrorsListItem'
            Prelude.<$> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "Page")
      )

instance Prelude.Hashable ErrorsListItem where
  hashWithSalt _salt ErrorsListItem' {..} =
    _salt
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` page

instance Prelude.NFData ErrorsListItem where
  rnf ErrorsListItem' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf page
