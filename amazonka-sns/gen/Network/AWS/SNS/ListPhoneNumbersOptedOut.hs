{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SNS.ListPhoneNumbersOptedOut
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of phone numbers that are opted out, meaning you cannot
-- send SMS messages to them.
--
-- The results for @ListPhoneNumbersOptedOut@ are paginated, and each page
-- returns up to 100 phone numbers. If additional phone numbers are
-- available after the first page of results, then a @NextToken@ string
-- will be returned. To receive the next page, you call
-- @ListPhoneNumbersOptedOut@ again using the @NextToken@ string received
-- from the previous call. When there are no more records to return,
-- @NextToken@ will be null.
--
-- This operation returns paginated results.
module Network.AWS.SNS.ListPhoneNumbersOptedOut
  ( -- * Creating a Request
    ListPhoneNumbersOptedOut (..),
    newListPhoneNumbersOptedOut,

    -- * Request Lenses
    listPhoneNumbersOptedOut_nextToken,

    -- * Destructuring the Response
    ListPhoneNumbersOptedOutResponse (..),
    newListPhoneNumbersOptedOutResponse,

    -- * Response Lenses
    listPhoneNumbersOptedOutResponse_nextToken,
    listPhoneNumbersOptedOutResponse_phoneNumbers,
    listPhoneNumbersOptedOutResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SNS.Types

-- | The input for the @ListPhoneNumbersOptedOut@ action.
--
-- /See:/ 'newListPhoneNumbersOptedOut' smart constructor.
data ListPhoneNumbersOptedOut = ListPhoneNumbersOptedOut'
  { -- | A @NextToken@ string is used when you call the
    -- @ListPhoneNumbersOptedOut@ action to retrieve additional records that
    -- are available after the first page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListPhoneNumbersOptedOut' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPhoneNumbersOptedOut_nextToken' - A @NextToken@ string is used when you call the
-- @ListPhoneNumbersOptedOut@ action to retrieve additional records that
-- are available after the first page of results.
newListPhoneNumbersOptedOut ::
  ListPhoneNumbersOptedOut
newListPhoneNumbersOptedOut =
  ListPhoneNumbersOptedOut'
    { nextToken =
        Prelude.Nothing
    }

-- | A @NextToken@ string is used when you call the
-- @ListPhoneNumbersOptedOut@ action to retrieve additional records that
-- are available after the first page of results.
listPhoneNumbersOptedOut_nextToken :: Lens.Lens' ListPhoneNumbersOptedOut (Prelude.Maybe Prelude.Text)
listPhoneNumbersOptedOut_nextToken = Lens.lens (\ListPhoneNumbersOptedOut' {nextToken} -> nextToken) (\s@ListPhoneNumbersOptedOut' {} a -> s {nextToken = a} :: ListPhoneNumbersOptedOut)

instance Pager.AWSPager ListPhoneNumbersOptedOut where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listPhoneNumbersOptedOutResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listPhoneNumbersOptedOutResponse_phoneNumbers
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listPhoneNumbersOptedOut_nextToken
          Lens..~ rs
          Lens.^? listPhoneNumbersOptedOutResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListPhoneNumbersOptedOut where
  type
    Rs ListPhoneNumbersOptedOut =
      ListPhoneNumbersOptedOutResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListPhoneNumbersOptedOutResult"
      ( \s h x ->
          ListPhoneNumbersOptedOutResponse'
            Prelude.<$> (x Prelude..@? "nextToken")
            Prelude.<*> ( x Prelude..@? "phoneNumbers"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPhoneNumbersOptedOut

instance Prelude.NFData ListPhoneNumbersOptedOut

instance Prelude.ToHeaders ListPhoneNumbersOptedOut where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ListPhoneNumbersOptedOut where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListPhoneNumbersOptedOut where
  toQuery ListPhoneNumbersOptedOut' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("ListPhoneNumbersOptedOut" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-03-31" :: Prelude.ByteString),
        "nextToken" Prelude.=: nextToken
      ]

-- | The response from the @ListPhoneNumbersOptedOut@ action.
--
-- /See:/ 'newListPhoneNumbersOptedOutResponse' smart constructor.
data ListPhoneNumbersOptedOutResponse = ListPhoneNumbersOptedOutResponse'
  { -- | A @NextToken@ string is returned when you call the
    -- @ListPhoneNumbersOptedOut@ action if additional records are available
    -- after the first page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of phone numbers that are opted out of receiving SMS messages.
    -- The list is paginated, and each page can contain up to 100 phone
    -- numbers.
    phoneNumbers :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListPhoneNumbersOptedOutResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPhoneNumbersOptedOutResponse_nextToken' - A @NextToken@ string is returned when you call the
-- @ListPhoneNumbersOptedOut@ action if additional records are available
-- after the first page of results.
--
-- 'phoneNumbers', 'listPhoneNumbersOptedOutResponse_phoneNumbers' - A list of phone numbers that are opted out of receiving SMS messages.
-- The list is paginated, and each page can contain up to 100 phone
-- numbers.
--
-- 'httpStatus', 'listPhoneNumbersOptedOutResponse_httpStatus' - The response's http status code.
newListPhoneNumbersOptedOutResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPhoneNumbersOptedOutResponse
newListPhoneNumbersOptedOutResponse pHttpStatus_ =
  ListPhoneNumbersOptedOutResponse'
    { nextToken =
        Prelude.Nothing,
      phoneNumbers = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A @NextToken@ string is returned when you call the
-- @ListPhoneNumbersOptedOut@ action if additional records are available
-- after the first page of results.
listPhoneNumbersOptedOutResponse_nextToken :: Lens.Lens' ListPhoneNumbersOptedOutResponse (Prelude.Maybe Prelude.Text)
listPhoneNumbersOptedOutResponse_nextToken = Lens.lens (\ListPhoneNumbersOptedOutResponse' {nextToken} -> nextToken) (\s@ListPhoneNumbersOptedOutResponse' {} a -> s {nextToken = a} :: ListPhoneNumbersOptedOutResponse)

-- | A list of phone numbers that are opted out of receiving SMS messages.
-- The list is paginated, and each page can contain up to 100 phone
-- numbers.
listPhoneNumbersOptedOutResponse_phoneNumbers :: Lens.Lens' ListPhoneNumbersOptedOutResponse (Prelude.Maybe [Prelude.Text])
listPhoneNumbersOptedOutResponse_phoneNumbers = Lens.lens (\ListPhoneNumbersOptedOutResponse' {phoneNumbers} -> phoneNumbers) (\s@ListPhoneNumbersOptedOutResponse' {} a -> s {phoneNumbers = a} :: ListPhoneNumbersOptedOutResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listPhoneNumbersOptedOutResponse_httpStatus :: Lens.Lens' ListPhoneNumbersOptedOutResponse Prelude.Int
listPhoneNumbersOptedOutResponse_httpStatus = Lens.lens (\ListPhoneNumbersOptedOutResponse' {httpStatus} -> httpStatus) (\s@ListPhoneNumbersOptedOutResponse' {} a -> s {httpStatus = a} :: ListPhoneNumbersOptedOutResponse)

instance
  Prelude.NFData
    ListPhoneNumbersOptedOutResponse
