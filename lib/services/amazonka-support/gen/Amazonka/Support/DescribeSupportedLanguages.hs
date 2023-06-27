{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Support.DescribeSupportedLanguages
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of supported languages for a specified @categoryCode@,
-- @issueType@ and @serviceCode@. The returned supported languages will
-- include a ISO 639-1 code for the @language@, and the language display
-- name.
--
-- -   You must have a Business, Enterprise On-Ramp, or Enterprise Support
--     plan to use the Amazon Web Services Support API.
--
-- -   If you call the Amazon Web Services Support API from an account that
--     doesn\'t have a Business, Enterprise On-Ramp, or Enterprise Support
--     plan, the @SubscriptionRequiredException@ error message appears. For
--     information about changing your support plan, see
--     <http://aws.amazon.com/premiumsupport/ Amazon Web Services Support>.
module Amazonka.Support.DescribeSupportedLanguages
  ( -- * Creating a Request
    DescribeSupportedLanguages (..),
    newDescribeSupportedLanguages,

    -- * Request Lenses
    describeSupportedLanguages_issueType,
    describeSupportedLanguages_serviceCode,
    describeSupportedLanguages_categoryCode,

    -- * Destructuring the Response
    DescribeSupportedLanguagesResponse (..),
    newDescribeSupportedLanguagesResponse,

    -- * Response Lenses
    describeSupportedLanguagesResponse_supportedLanguages,
    describeSupportedLanguagesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Support.Types

-- | /See:/ 'newDescribeSupportedLanguages' smart constructor.
data DescribeSupportedLanguages = DescribeSupportedLanguages'
  { -- | The type of issue for the case. You can specify @customer-service@ or
    -- @technical@.
    issueType :: Prelude.Text,
    -- | The code for the Amazon Web Services service. You can use the
    -- DescribeServices operation to get the possible @serviceCode@ values.
    serviceCode :: Prelude.Text,
    -- | The category of problem for the support case. You also use the
    -- DescribeServices operation to get the category code for a service. Each
    -- Amazon Web Services service defines its own set of category codes.
    categoryCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSupportedLanguages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'issueType', 'describeSupportedLanguages_issueType' - The type of issue for the case. You can specify @customer-service@ or
-- @technical@.
--
-- 'serviceCode', 'describeSupportedLanguages_serviceCode' - The code for the Amazon Web Services service. You can use the
-- DescribeServices operation to get the possible @serviceCode@ values.
--
-- 'categoryCode', 'describeSupportedLanguages_categoryCode' - The category of problem for the support case. You also use the
-- DescribeServices operation to get the category code for a service. Each
-- Amazon Web Services service defines its own set of category codes.
newDescribeSupportedLanguages ::
  -- | 'issueType'
  Prelude.Text ->
  -- | 'serviceCode'
  Prelude.Text ->
  -- | 'categoryCode'
  Prelude.Text ->
  DescribeSupportedLanguages
newDescribeSupportedLanguages
  pIssueType_
  pServiceCode_
  pCategoryCode_ =
    DescribeSupportedLanguages'
      { issueType =
          pIssueType_,
        serviceCode = pServiceCode_,
        categoryCode = pCategoryCode_
      }

-- | The type of issue for the case. You can specify @customer-service@ or
-- @technical@.
describeSupportedLanguages_issueType :: Lens.Lens' DescribeSupportedLanguages Prelude.Text
describeSupportedLanguages_issueType = Lens.lens (\DescribeSupportedLanguages' {issueType} -> issueType) (\s@DescribeSupportedLanguages' {} a -> s {issueType = a} :: DescribeSupportedLanguages)

-- | The code for the Amazon Web Services service. You can use the
-- DescribeServices operation to get the possible @serviceCode@ values.
describeSupportedLanguages_serviceCode :: Lens.Lens' DescribeSupportedLanguages Prelude.Text
describeSupportedLanguages_serviceCode = Lens.lens (\DescribeSupportedLanguages' {serviceCode} -> serviceCode) (\s@DescribeSupportedLanguages' {} a -> s {serviceCode = a} :: DescribeSupportedLanguages)

-- | The category of problem for the support case. You also use the
-- DescribeServices operation to get the category code for a service. Each
-- Amazon Web Services service defines its own set of category codes.
describeSupportedLanguages_categoryCode :: Lens.Lens' DescribeSupportedLanguages Prelude.Text
describeSupportedLanguages_categoryCode = Lens.lens (\DescribeSupportedLanguages' {categoryCode} -> categoryCode) (\s@DescribeSupportedLanguages' {} a -> s {categoryCode = a} :: DescribeSupportedLanguages)

instance Core.AWSRequest DescribeSupportedLanguages where
  type
    AWSResponse DescribeSupportedLanguages =
      DescribeSupportedLanguagesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSupportedLanguagesResponse'
            Prelude.<$> ( x
                            Data..?> "supportedLanguages"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSupportedLanguages where
  hashWithSalt _salt DescribeSupportedLanguages' {..} =
    _salt
      `Prelude.hashWithSalt` issueType
      `Prelude.hashWithSalt` serviceCode
      `Prelude.hashWithSalt` categoryCode

instance Prelude.NFData DescribeSupportedLanguages where
  rnf DescribeSupportedLanguages' {..} =
    Prelude.rnf issueType
      `Prelude.seq` Prelude.rnf serviceCode
      `Prelude.seq` Prelude.rnf categoryCode

instance Data.ToHeaders DescribeSupportedLanguages where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSSupport_20130415.DescribeSupportedLanguages" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeSupportedLanguages where
  toJSON DescribeSupportedLanguages' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("issueType" Data..= issueType),
            Prelude.Just ("serviceCode" Data..= serviceCode),
            Prelude.Just ("categoryCode" Data..= categoryCode)
          ]
      )

instance Data.ToPath DescribeSupportedLanguages where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeSupportedLanguages where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSupportedLanguagesResponse' smart constructor.
data DescribeSupportedLanguagesResponse = DescribeSupportedLanguagesResponse'
  { -- | A JSON-formatted array that contains the available ISO 639-1 language
    -- codes.
    supportedLanguages :: Prelude.Maybe [SupportedLanguage],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSupportedLanguagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'supportedLanguages', 'describeSupportedLanguagesResponse_supportedLanguages' - A JSON-formatted array that contains the available ISO 639-1 language
-- codes.
--
-- 'httpStatus', 'describeSupportedLanguagesResponse_httpStatus' - The response's http status code.
newDescribeSupportedLanguagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSupportedLanguagesResponse
newDescribeSupportedLanguagesResponse pHttpStatus_ =
  DescribeSupportedLanguagesResponse'
    { supportedLanguages =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A JSON-formatted array that contains the available ISO 639-1 language
-- codes.
describeSupportedLanguagesResponse_supportedLanguages :: Lens.Lens' DescribeSupportedLanguagesResponse (Prelude.Maybe [SupportedLanguage])
describeSupportedLanguagesResponse_supportedLanguages = Lens.lens (\DescribeSupportedLanguagesResponse' {supportedLanguages} -> supportedLanguages) (\s@DescribeSupportedLanguagesResponse' {} a -> s {supportedLanguages = a} :: DescribeSupportedLanguagesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeSupportedLanguagesResponse_httpStatus :: Lens.Lens' DescribeSupportedLanguagesResponse Prelude.Int
describeSupportedLanguagesResponse_httpStatus = Lens.lens (\DescribeSupportedLanguagesResponse' {httpStatus} -> httpStatus) (\s@DescribeSupportedLanguagesResponse' {} a -> s {httpStatus = a} :: DescribeSupportedLanguagesResponse)

instance
  Prelude.NFData
    DescribeSupportedLanguagesResponse
  where
  rnf DescribeSupportedLanguagesResponse' {..} =
    Prelude.rnf supportedLanguages
      `Prelude.seq` Prelude.rnf httpStatus
