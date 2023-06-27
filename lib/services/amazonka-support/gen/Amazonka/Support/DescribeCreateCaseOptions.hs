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
-- Module      : Amazonka.Support.DescribeCreateCaseOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of CreateCaseOption types along with the corresponding
-- supported hours and language availability. You can specify the
-- @language@ @categoryCode@, @issueType@ and @serviceCode@ used to
-- retrieve the CreateCaseOptions.
--
-- -   You must have a Business, Enterprise On-Ramp, or Enterprise Support
--     plan to use the Amazon Web Services Support API.
--
-- -   If you call the Amazon Web Services Support API from an account that
--     doesn\'t have a Business, Enterprise On-Ramp, or Enterprise Support
--     plan, the @SubscriptionRequiredException@ error message appears. For
--     information about changing your support plan, see
--     <http://aws.amazon.com/premiumsupport/ Amazon Web Services Support>.
module Amazonka.Support.DescribeCreateCaseOptions
  ( -- * Creating a Request
    DescribeCreateCaseOptions (..),
    newDescribeCreateCaseOptions,

    -- * Request Lenses
    describeCreateCaseOptions_issueType,
    describeCreateCaseOptions_serviceCode,
    describeCreateCaseOptions_language,
    describeCreateCaseOptions_categoryCode,

    -- * Destructuring the Response
    DescribeCreateCaseOptionsResponse (..),
    newDescribeCreateCaseOptionsResponse,

    -- * Response Lenses
    describeCreateCaseOptionsResponse_communicationTypes,
    describeCreateCaseOptionsResponse_languageAvailability,
    describeCreateCaseOptionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Support.Types

-- | /See:/ 'newDescribeCreateCaseOptions' smart constructor.
data DescribeCreateCaseOptions = DescribeCreateCaseOptions'
  { -- | The type of issue for the case. You can specify @customer-service@ or
    -- @technical@. If you don\'t specify a value, the default is @technical@.
    issueType :: Prelude.Text,
    -- | The code for the Amazon Web Services service. You can use the
    -- DescribeServices operation to get the possible @serviceCode@ values.
    serviceCode :: Prelude.Text,
    -- | The language in which Amazon Web Services Support handles the case.
    -- Amazon Web Services Support currently supports Chinese (“zh”), English
    -- (\"en\"), Japanese (\"ja\") and Korean (“ko”). You must specify the ISO
    -- 639-1 code for the @language@ parameter if you want support in that
    -- language.
    language :: Prelude.Text,
    -- | The category of problem for the support case. You also use the
    -- DescribeServices operation to get the category code for a service. Each
    -- Amazon Web Services service defines its own set of category codes.
    categoryCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCreateCaseOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'issueType', 'describeCreateCaseOptions_issueType' - The type of issue for the case. You can specify @customer-service@ or
-- @technical@. If you don\'t specify a value, the default is @technical@.
--
-- 'serviceCode', 'describeCreateCaseOptions_serviceCode' - The code for the Amazon Web Services service. You can use the
-- DescribeServices operation to get the possible @serviceCode@ values.
--
-- 'language', 'describeCreateCaseOptions_language' - The language in which Amazon Web Services Support handles the case.
-- Amazon Web Services Support currently supports Chinese (“zh”), English
-- (\"en\"), Japanese (\"ja\") and Korean (“ko”). You must specify the ISO
-- 639-1 code for the @language@ parameter if you want support in that
-- language.
--
-- 'categoryCode', 'describeCreateCaseOptions_categoryCode' - The category of problem for the support case. You also use the
-- DescribeServices operation to get the category code for a service. Each
-- Amazon Web Services service defines its own set of category codes.
newDescribeCreateCaseOptions ::
  -- | 'issueType'
  Prelude.Text ->
  -- | 'serviceCode'
  Prelude.Text ->
  -- | 'language'
  Prelude.Text ->
  -- | 'categoryCode'
  Prelude.Text ->
  DescribeCreateCaseOptions
newDescribeCreateCaseOptions
  pIssueType_
  pServiceCode_
  pLanguage_
  pCategoryCode_ =
    DescribeCreateCaseOptions'
      { issueType = pIssueType_,
        serviceCode = pServiceCode_,
        language = pLanguage_,
        categoryCode = pCategoryCode_
      }

-- | The type of issue for the case. You can specify @customer-service@ or
-- @technical@. If you don\'t specify a value, the default is @technical@.
describeCreateCaseOptions_issueType :: Lens.Lens' DescribeCreateCaseOptions Prelude.Text
describeCreateCaseOptions_issueType = Lens.lens (\DescribeCreateCaseOptions' {issueType} -> issueType) (\s@DescribeCreateCaseOptions' {} a -> s {issueType = a} :: DescribeCreateCaseOptions)

-- | The code for the Amazon Web Services service. You can use the
-- DescribeServices operation to get the possible @serviceCode@ values.
describeCreateCaseOptions_serviceCode :: Lens.Lens' DescribeCreateCaseOptions Prelude.Text
describeCreateCaseOptions_serviceCode = Lens.lens (\DescribeCreateCaseOptions' {serviceCode} -> serviceCode) (\s@DescribeCreateCaseOptions' {} a -> s {serviceCode = a} :: DescribeCreateCaseOptions)

-- | The language in which Amazon Web Services Support handles the case.
-- Amazon Web Services Support currently supports Chinese (“zh”), English
-- (\"en\"), Japanese (\"ja\") and Korean (“ko”). You must specify the ISO
-- 639-1 code for the @language@ parameter if you want support in that
-- language.
describeCreateCaseOptions_language :: Lens.Lens' DescribeCreateCaseOptions Prelude.Text
describeCreateCaseOptions_language = Lens.lens (\DescribeCreateCaseOptions' {language} -> language) (\s@DescribeCreateCaseOptions' {} a -> s {language = a} :: DescribeCreateCaseOptions)

-- | The category of problem for the support case. You also use the
-- DescribeServices operation to get the category code for a service. Each
-- Amazon Web Services service defines its own set of category codes.
describeCreateCaseOptions_categoryCode :: Lens.Lens' DescribeCreateCaseOptions Prelude.Text
describeCreateCaseOptions_categoryCode = Lens.lens (\DescribeCreateCaseOptions' {categoryCode} -> categoryCode) (\s@DescribeCreateCaseOptions' {} a -> s {categoryCode = a} :: DescribeCreateCaseOptions)

instance Core.AWSRequest DescribeCreateCaseOptions where
  type
    AWSResponse DescribeCreateCaseOptions =
      DescribeCreateCaseOptionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCreateCaseOptionsResponse'
            Prelude.<$> (x Data..?> "communicationTypes")
            Prelude.<*> (x Data..?> "languageAvailability")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCreateCaseOptions where
  hashWithSalt _salt DescribeCreateCaseOptions' {..} =
    _salt
      `Prelude.hashWithSalt` issueType
      `Prelude.hashWithSalt` serviceCode
      `Prelude.hashWithSalt` language
      `Prelude.hashWithSalt` categoryCode

instance Prelude.NFData DescribeCreateCaseOptions where
  rnf DescribeCreateCaseOptions' {..} =
    Prelude.rnf issueType
      `Prelude.seq` Prelude.rnf serviceCode
      `Prelude.seq` Prelude.rnf language
      `Prelude.seq` Prelude.rnf categoryCode

instance Data.ToHeaders DescribeCreateCaseOptions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSSupport_20130415.DescribeCreateCaseOptions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeCreateCaseOptions where
  toJSON DescribeCreateCaseOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("issueType" Data..= issueType),
            Prelude.Just ("serviceCode" Data..= serviceCode),
            Prelude.Just ("language" Data..= language),
            Prelude.Just ("categoryCode" Data..= categoryCode)
          ]
      )

instance Data.ToPath DescribeCreateCaseOptions where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeCreateCaseOptions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeCreateCaseOptionsResponse' smart constructor.
data DescribeCreateCaseOptionsResponse = DescribeCreateCaseOptionsResponse'
  { -- | A JSON-formatted array that contains the available communication type
    -- options, along with the available support timeframes for the given
    -- inputs.
    communicationTypes :: Prelude.Maybe (Prelude.NonEmpty CommunicationTypeOptions),
    -- | Language availability can be any of the following:
    --
    -- -   available
    --
    -- -   best_effort
    --
    -- -   unavailable
    languageAvailability :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCreateCaseOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'communicationTypes', 'describeCreateCaseOptionsResponse_communicationTypes' - A JSON-formatted array that contains the available communication type
-- options, along with the available support timeframes for the given
-- inputs.
--
-- 'languageAvailability', 'describeCreateCaseOptionsResponse_languageAvailability' - Language availability can be any of the following:
--
-- -   available
--
-- -   best_effort
--
-- -   unavailable
--
-- 'httpStatus', 'describeCreateCaseOptionsResponse_httpStatus' - The response's http status code.
newDescribeCreateCaseOptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCreateCaseOptionsResponse
newDescribeCreateCaseOptionsResponse pHttpStatus_ =
  DescribeCreateCaseOptionsResponse'
    { communicationTypes =
        Prelude.Nothing,
      languageAvailability = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A JSON-formatted array that contains the available communication type
-- options, along with the available support timeframes for the given
-- inputs.
describeCreateCaseOptionsResponse_communicationTypes :: Lens.Lens' DescribeCreateCaseOptionsResponse (Prelude.Maybe (Prelude.NonEmpty CommunicationTypeOptions))
describeCreateCaseOptionsResponse_communicationTypes = Lens.lens (\DescribeCreateCaseOptionsResponse' {communicationTypes} -> communicationTypes) (\s@DescribeCreateCaseOptionsResponse' {} a -> s {communicationTypes = a} :: DescribeCreateCaseOptionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Language availability can be any of the following:
--
-- -   available
--
-- -   best_effort
--
-- -   unavailable
describeCreateCaseOptionsResponse_languageAvailability :: Lens.Lens' DescribeCreateCaseOptionsResponse (Prelude.Maybe Prelude.Text)
describeCreateCaseOptionsResponse_languageAvailability = Lens.lens (\DescribeCreateCaseOptionsResponse' {languageAvailability} -> languageAvailability) (\s@DescribeCreateCaseOptionsResponse' {} a -> s {languageAvailability = a} :: DescribeCreateCaseOptionsResponse)

-- | The response's http status code.
describeCreateCaseOptionsResponse_httpStatus :: Lens.Lens' DescribeCreateCaseOptionsResponse Prelude.Int
describeCreateCaseOptionsResponse_httpStatus = Lens.lens (\DescribeCreateCaseOptionsResponse' {httpStatus} -> httpStatus) (\s@DescribeCreateCaseOptionsResponse' {} a -> s {httpStatus = a} :: DescribeCreateCaseOptionsResponse)

instance
  Prelude.NFData
    DescribeCreateCaseOptionsResponse
  where
  rnf DescribeCreateCaseOptionsResponse' {..} =
    Prelude.rnf communicationTypes
      `Prelude.seq` Prelude.rnf languageAvailability
      `Prelude.seq` Prelude.rnf httpStatus
