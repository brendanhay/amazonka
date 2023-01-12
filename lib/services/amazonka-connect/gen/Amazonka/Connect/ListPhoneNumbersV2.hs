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
-- Module      : Amazonka.Connect.ListPhoneNumbersV2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists phone numbers claimed to your Amazon Connect instance or traffic
-- distribution group. If the provided @TargetArn@ is a traffic
-- distribution group, you can call this API in both Amazon Web Services
-- Regions associated with traffic distribution group.
--
-- For more information about phone numbers, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/contact-center-phone-number.html Set Up Phone Numbers for Your Contact Center>
-- in the /Amazon Connect Administrator Guide/.
--
-- This operation returns paginated results.
module Amazonka.Connect.ListPhoneNumbersV2
  ( -- * Creating a Request
    ListPhoneNumbersV2 (..),
    newListPhoneNumbersV2,

    -- * Request Lenses
    listPhoneNumbersV2_maxResults,
    listPhoneNumbersV2_nextToken,
    listPhoneNumbersV2_phoneNumberCountryCodes,
    listPhoneNumbersV2_phoneNumberPrefix,
    listPhoneNumbersV2_phoneNumberTypes,
    listPhoneNumbersV2_targetArn,

    -- * Destructuring the Response
    ListPhoneNumbersV2Response (..),
    newListPhoneNumbersV2Response,

    -- * Response Lenses
    listPhoneNumbersV2Response_listPhoneNumbersSummaryList,
    listPhoneNumbersV2Response_nextToken,
    listPhoneNumbersV2Response_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPhoneNumbersV2' smart constructor.
data ListPhoneNumbersV2 = ListPhoneNumbersV2'
  { -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ISO country code.
    phoneNumberCountryCodes :: Prelude.Maybe [PhoneNumberCountryCode],
    -- | The prefix of the phone number. If provided, it must contain @+@ as part
    -- of the country code.
    phoneNumberPrefix :: Prelude.Maybe Prelude.Text,
    -- | The type of phone number.
    phoneNumberTypes :: Prelude.Maybe [PhoneNumberType],
    -- | The Amazon Resource Name (ARN) for Amazon Connect instances or traffic
    -- distribution groups that phone numbers are claimed to. If @TargetArn@
    -- input is not provided, this API lists numbers claimed to all the Amazon
    -- Connect instances belonging to your account in the same Amazon Web
    -- Services Region as the request.
    targetArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPhoneNumbersV2' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listPhoneNumbersV2_maxResults' - The maximum number of results to return per page.
--
-- 'nextToken', 'listPhoneNumbersV2_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'phoneNumberCountryCodes', 'listPhoneNumbersV2_phoneNumberCountryCodes' - The ISO country code.
--
-- 'phoneNumberPrefix', 'listPhoneNumbersV2_phoneNumberPrefix' - The prefix of the phone number. If provided, it must contain @+@ as part
-- of the country code.
--
-- 'phoneNumberTypes', 'listPhoneNumbersV2_phoneNumberTypes' - The type of phone number.
--
-- 'targetArn', 'listPhoneNumbersV2_targetArn' - The Amazon Resource Name (ARN) for Amazon Connect instances or traffic
-- distribution groups that phone numbers are claimed to. If @TargetArn@
-- input is not provided, this API lists numbers claimed to all the Amazon
-- Connect instances belonging to your account in the same Amazon Web
-- Services Region as the request.
newListPhoneNumbersV2 ::
  ListPhoneNumbersV2
newListPhoneNumbersV2 =
  ListPhoneNumbersV2'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      phoneNumberCountryCodes = Prelude.Nothing,
      phoneNumberPrefix = Prelude.Nothing,
      phoneNumberTypes = Prelude.Nothing,
      targetArn = Prelude.Nothing
    }

-- | The maximum number of results to return per page.
listPhoneNumbersV2_maxResults :: Lens.Lens' ListPhoneNumbersV2 (Prelude.Maybe Prelude.Natural)
listPhoneNumbersV2_maxResults = Lens.lens (\ListPhoneNumbersV2' {maxResults} -> maxResults) (\s@ListPhoneNumbersV2' {} a -> s {maxResults = a} :: ListPhoneNumbersV2)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listPhoneNumbersV2_nextToken :: Lens.Lens' ListPhoneNumbersV2 (Prelude.Maybe Prelude.Text)
listPhoneNumbersV2_nextToken = Lens.lens (\ListPhoneNumbersV2' {nextToken} -> nextToken) (\s@ListPhoneNumbersV2' {} a -> s {nextToken = a} :: ListPhoneNumbersV2)

-- | The ISO country code.
listPhoneNumbersV2_phoneNumberCountryCodes :: Lens.Lens' ListPhoneNumbersV2 (Prelude.Maybe [PhoneNumberCountryCode])
listPhoneNumbersV2_phoneNumberCountryCodes = Lens.lens (\ListPhoneNumbersV2' {phoneNumberCountryCodes} -> phoneNumberCountryCodes) (\s@ListPhoneNumbersV2' {} a -> s {phoneNumberCountryCodes = a} :: ListPhoneNumbersV2) Prelude.. Lens.mapping Lens.coerced

-- | The prefix of the phone number. If provided, it must contain @+@ as part
-- of the country code.
listPhoneNumbersV2_phoneNumberPrefix :: Lens.Lens' ListPhoneNumbersV2 (Prelude.Maybe Prelude.Text)
listPhoneNumbersV2_phoneNumberPrefix = Lens.lens (\ListPhoneNumbersV2' {phoneNumberPrefix} -> phoneNumberPrefix) (\s@ListPhoneNumbersV2' {} a -> s {phoneNumberPrefix = a} :: ListPhoneNumbersV2)

-- | The type of phone number.
listPhoneNumbersV2_phoneNumberTypes :: Lens.Lens' ListPhoneNumbersV2 (Prelude.Maybe [PhoneNumberType])
listPhoneNumbersV2_phoneNumberTypes = Lens.lens (\ListPhoneNumbersV2' {phoneNumberTypes} -> phoneNumberTypes) (\s@ListPhoneNumbersV2' {} a -> s {phoneNumberTypes = a} :: ListPhoneNumbersV2) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) for Amazon Connect instances or traffic
-- distribution groups that phone numbers are claimed to. If @TargetArn@
-- input is not provided, this API lists numbers claimed to all the Amazon
-- Connect instances belonging to your account in the same Amazon Web
-- Services Region as the request.
listPhoneNumbersV2_targetArn :: Lens.Lens' ListPhoneNumbersV2 (Prelude.Maybe Prelude.Text)
listPhoneNumbersV2_targetArn = Lens.lens (\ListPhoneNumbersV2' {targetArn} -> targetArn) (\s@ListPhoneNumbersV2' {} a -> s {targetArn = a} :: ListPhoneNumbersV2)

instance Core.AWSPager ListPhoneNumbersV2 where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPhoneNumbersV2Response_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPhoneNumbersV2Response_listPhoneNumbersSummaryList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listPhoneNumbersV2_nextToken
          Lens..~ rs
          Lens.^? listPhoneNumbersV2Response_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListPhoneNumbersV2 where
  type
    AWSResponse ListPhoneNumbersV2 =
      ListPhoneNumbersV2Response
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPhoneNumbersV2Response'
            Prelude.<$> ( x Data..?> "ListPhoneNumbersSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPhoneNumbersV2 where
  hashWithSalt _salt ListPhoneNumbersV2' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` phoneNumberCountryCodes
      `Prelude.hashWithSalt` phoneNumberPrefix
      `Prelude.hashWithSalt` phoneNumberTypes
      `Prelude.hashWithSalt` targetArn

instance Prelude.NFData ListPhoneNumbersV2 where
  rnf ListPhoneNumbersV2' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf phoneNumberCountryCodes
      `Prelude.seq` Prelude.rnf phoneNumberPrefix
      `Prelude.seq` Prelude.rnf phoneNumberTypes
      `Prelude.seq` Prelude.rnf targetArn

instance Data.ToHeaders ListPhoneNumbersV2 where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListPhoneNumbersV2 where
  toJSON ListPhoneNumbersV2' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("PhoneNumberCountryCodes" Data..=)
              Prelude.<$> phoneNumberCountryCodes,
            ("PhoneNumberPrefix" Data..=)
              Prelude.<$> phoneNumberPrefix,
            ("PhoneNumberTypes" Data..=)
              Prelude.<$> phoneNumberTypes,
            ("TargetArn" Data..=) Prelude.<$> targetArn
          ]
      )

instance Data.ToPath ListPhoneNumbersV2 where
  toPath = Prelude.const "/phone-number/list"

instance Data.ToQuery ListPhoneNumbersV2 where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPhoneNumbersV2Response' smart constructor.
data ListPhoneNumbersV2Response = ListPhoneNumbersV2Response'
  { -- | Information about phone numbers that have been claimed to your Amazon
    -- Connect instances or traffic distribution groups.
    listPhoneNumbersSummaryList :: Prelude.Maybe [ListPhoneNumbersSummary],
    -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPhoneNumbersV2Response' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'listPhoneNumbersSummaryList', 'listPhoneNumbersV2Response_listPhoneNumbersSummaryList' - Information about phone numbers that have been claimed to your Amazon
-- Connect instances or traffic distribution groups.
--
-- 'nextToken', 'listPhoneNumbersV2Response_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'httpStatus', 'listPhoneNumbersV2Response_httpStatus' - The response's http status code.
newListPhoneNumbersV2Response ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPhoneNumbersV2Response
newListPhoneNumbersV2Response pHttpStatus_ =
  ListPhoneNumbersV2Response'
    { listPhoneNumbersSummaryList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about phone numbers that have been claimed to your Amazon
-- Connect instances or traffic distribution groups.
listPhoneNumbersV2Response_listPhoneNumbersSummaryList :: Lens.Lens' ListPhoneNumbersV2Response (Prelude.Maybe [ListPhoneNumbersSummary])
listPhoneNumbersV2Response_listPhoneNumbersSummaryList = Lens.lens (\ListPhoneNumbersV2Response' {listPhoneNumbersSummaryList} -> listPhoneNumbersSummaryList) (\s@ListPhoneNumbersV2Response' {} a -> s {listPhoneNumbersSummaryList = a} :: ListPhoneNumbersV2Response) Prelude.. Lens.mapping Lens.coerced

-- | If there are additional results, this is the token for the next set of
-- results.
listPhoneNumbersV2Response_nextToken :: Lens.Lens' ListPhoneNumbersV2Response (Prelude.Maybe Prelude.Text)
listPhoneNumbersV2Response_nextToken = Lens.lens (\ListPhoneNumbersV2Response' {nextToken} -> nextToken) (\s@ListPhoneNumbersV2Response' {} a -> s {nextToken = a} :: ListPhoneNumbersV2Response)

-- | The response's http status code.
listPhoneNumbersV2Response_httpStatus :: Lens.Lens' ListPhoneNumbersV2Response Prelude.Int
listPhoneNumbersV2Response_httpStatus = Lens.lens (\ListPhoneNumbersV2Response' {httpStatus} -> httpStatus) (\s@ListPhoneNumbersV2Response' {} a -> s {httpStatus = a} :: ListPhoneNumbersV2Response)

instance Prelude.NFData ListPhoneNumbersV2Response where
  rnf ListPhoneNumbersV2Response' {..} =
    Prelude.rnf listPhoneNumbersSummaryList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
