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
-- Module      : Amazonka.SNS.ListSMSSandboxPhoneNumbers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the calling Amazon Web Services account\'s current verified and
-- pending destination phone numbers in the SMS sandbox.
--
-- When you start using Amazon SNS to send SMS messages, your Amazon Web
-- Services account is in the /SMS sandbox/. The SMS sandbox provides a
-- safe environment for you to try Amazon SNS features without risking your
-- reputation as an SMS sender. While your Amazon Web Services account is
-- in the SMS sandbox, you can use all of the features of Amazon SNS.
-- However, you can send SMS messages only to verified destination phone
-- numbers. For more information, including how to move out of the sandbox
-- to send messages without restrictions, see
-- <https://docs.aws.amazon.com/sns/latest/dg/sns-sms-sandbox.html SMS sandbox>
-- in the /Amazon SNS Developer Guide/.
--
-- This operation returns paginated results.
module Amazonka.SNS.ListSMSSandboxPhoneNumbers
  ( -- * Creating a Request
    ListSMSSandboxPhoneNumbers (..),
    newListSMSSandboxPhoneNumbers,

    -- * Request Lenses
    listSMSSandboxPhoneNumbers_maxResults,
    listSMSSandboxPhoneNumbers_nextToken,

    -- * Destructuring the Response
    ListSMSSandboxPhoneNumbersResponse (..),
    newListSMSSandboxPhoneNumbersResponse,

    -- * Response Lenses
    listSMSSandboxPhoneNumbersResponse_nextToken,
    listSMSSandboxPhoneNumbersResponse_httpStatus,
    listSMSSandboxPhoneNumbersResponse_phoneNumbers,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SNS.Types

-- | /See:/ 'newListSMSSandboxPhoneNumbers' smart constructor.
data ListSMSSandboxPhoneNumbers = ListSMSSandboxPhoneNumbers'
  { -- | The maximum number of phone numbers to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Token that the previous @ListSMSSandboxPhoneNumbersInput@ request
    -- returns.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSMSSandboxPhoneNumbers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listSMSSandboxPhoneNumbers_maxResults' - The maximum number of phone numbers to return.
--
-- 'nextToken', 'listSMSSandboxPhoneNumbers_nextToken' - Token that the previous @ListSMSSandboxPhoneNumbersInput@ request
-- returns.
newListSMSSandboxPhoneNumbers ::
  ListSMSSandboxPhoneNumbers
newListSMSSandboxPhoneNumbers =
  ListSMSSandboxPhoneNumbers'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of phone numbers to return.
listSMSSandboxPhoneNumbers_maxResults :: Lens.Lens' ListSMSSandboxPhoneNumbers (Prelude.Maybe Prelude.Natural)
listSMSSandboxPhoneNumbers_maxResults = Lens.lens (\ListSMSSandboxPhoneNumbers' {maxResults} -> maxResults) (\s@ListSMSSandboxPhoneNumbers' {} a -> s {maxResults = a} :: ListSMSSandboxPhoneNumbers)

-- | Token that the previous @ListSMSSandboxPhoneNumbersInput@ request
-- returns.
listSMSSandboxPhoneNumbers_nextToken :: Lens.Lens' ListSMSSandboxPhoneNumbers (Prelude.Maybe Prelude.Text)
listSMSSandboxPhoneNumbers_nextToken = Lens.lens (\ListSMSSandboxPhoneNumbers' {nextToken} -> nextToken) (\s@ListSMSSandboxPhoneNumbers' {} a -> s {nextToken = a} :: ListSMSSandboxPhoneNumbers)

instance Core.AWSPager ListSMSSandboxPhoneNumbers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSMSSandboxPhoneNumbersResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listSMSSandboxPhoneNumbersResponse_phoneNumbers
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listSMSSandboxPhoneNumbers_nextToken
          Lens..~ rs
          Lens.^? listSMSSandboxPhoneNumbersResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListSMSSandboxPhoneNumbers where
  type
    AWSResponse ListSMSSandboxPhoneNumbers =
      ListSMSSandboxPhoneNumbersResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListSMSSandboxPhoneNumbersResult"
      ( \s h x ->
          ListSMSSandboxPhoneNumbersResponse'
            Prelude.<$> (x Data..@? "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..@? "PhoneNumbers"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Data.parseXMLList "member"
                        )
      )

instance Prelude.Hashable ListSMSSandboxPhoneNumbers where
  hashWithSalt _salt ListSMSSandboxPhoneNumbers' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListSMSSandboxPhoneNumbers where
  rnf ListSMSSandboxPhoneNumbers' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListSMSSandboxPhoneNumbers where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListSMSSandboxPhoneNumbers where
  toPath = Prelude.const "/"

instance Data.ToQuery ListSMSSandboxPhoneNumbers where
  toQuery ListSMSSandboxPhoneNumbers' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ListSMSSandboxPhoneNumbers" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-03-31" :: Prelude.ByteString),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListSMSSandboxPhoneNumbersResponse' smart constructor.
data ListSMSSandboxPhoneNumbersResponse = ListSMSSandboxPhoneNumbersResponse'
  { -- | A @NextToken@ string is returned when you call the
    -- @ListSMSSandboxPhoneNumbersInput@ operation if additional pages of
    -- records are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of the calling account\'s pending and verified phone numbers.
    phoneNumbers :: [SMSSandboxPhoneNumber]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSMSSandboxPhoneNumbersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSMSSandboxPhoneNumbersResponse_nextToken' - A @NextToken@ string is returned when you call the
-- @ListSMSSandboxPhoneNumbersInput@ operation if additional pages of
-- records are available.
--
-- 'httpStatus', 'listSMSSandboxPhoneNumbersResponse_httpStatus' - The response's http status code.
--
-- 'phoneNumbers', 'listSMSSandboxPhoneNumbersResponse_phoneNumbers' - A list of the calling account\'s pending and verified phone numbers.
newListSMSSandboxPhoneNumbersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSMSSandboxPhoneNumbersResponse
newListSMSSandboxPhoneNumbersResponse pHttpStatus_ =
  ListSMSSandboxPhoneNumbersResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      phoneNumbers = Prelude.mempty
    }

-- | A @NextToken@ string is returned when you call the
-- @ListSMSSandboxPhoneNumbersInput@ operation if additional pages of
-- records are available.
listSMSSandboxPhoneNumbersResponse_nextToken :: Lens.Lens' ListSMSSandboxPhoneNumbersResponse (Prelude.Maybe Prelude.Text)
listSMSSandboxPhoneNumbersResponse_nextToken = Lens.lens (\ListSMSSandboxPhoneNumbersResponse' {nextToken} -> nextToken) (\s@ListSMSSandboxPhoneNumbersResponse' {} a -> s {nextToken = a} :: ListSMSSandboxPhoneNumbersResponse)

-- | The response's http status code.
listSMSSandboxPhoneNumbersResponse_httpStatus :: Lens.Lens' ListSMSSandboxPhoneNumbersResponse Prelude.Int
listSMSSandboxPhoneNumbersResponse_httpStatus = Lens.lens (\ListSMSSandboxPhoneNumbersResponse' {httpStatus} -> httpStatus) (\s@ListSMSSandboxPhoneNumbersResponse' {} a -> s {httpStatus = a} :: ListSMSSandboxPhoneNumbersResponse)

-- | A list of the calling account\'s pending and verified phone numbers.
listSMSSandboxPhoneNumbersResponse_phoneNumbers :: Lens.Lens' ListSMSSandboxPhoneNumbersResponse [SMSSandboxPhoneNumber]
listSMSSandboxPhoneNumbersResponse_phoneNumbers = Lens.lens (\ListSMSSandboxPhoneNumbersResponse' {phoneNumbers} -> phoneNumbers) (\s@ListSMSSandboxPhoneNumbersResponse' {} a -> s {phoneNumbers = a} :: ListSMSSandboxPhoneNumbersResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListSMSSandboxPhoneNumbersResponse
  where
  rnf ListSMSSandboxPhoneNumbersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf phoneNumbers
