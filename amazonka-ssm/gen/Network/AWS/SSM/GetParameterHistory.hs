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
-- Module      : Network.AWS.SSM.GetParameterHistory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the history of all changes to a parameter.
--
-- This operation returns paginated results.
module Network.AWS.SSM.GetParameterHistory
  ( -- * Creating a Request
    GetParameterHistory (..),
    newGetParameterHistory,

    -- * Request Lenses
    getParameterHistory_withDecryption,
    getParameterHistory_nextToken,
    getParameterHistory_maxResults,
    getParameterHistory_name,

    -- * Destructuring the Response
    GetParameterHistoryResponse (..),
    newGetParameterHistoryResponse,

    -- * Response Lenses
    getParameterHistoryResponse_nextToken,
    getParameterHistoryResponse_parameters,
    getParameterHistoryResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newGetParameterHistory' smart constructor.
data GetParameterHistory = GetParameterHistory'
  { -- | Return decrypted values for secure string parameters. This flag is
    -- ignored for String and StringList parameter types.
    withDecryption :: Prelude.Maybe Prelude.Bool,
    -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the parameter for which you want to review history.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetParameterHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'withDecryption', 'getParameterHistory_withDecryption' - Return decrypted values for secure string parameters. This flag is
-- ignored for String and StringList parameter types.
--
-- 'nextToken', 'getParameterHistory_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'maxResults', 'getParameterHistory_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'name', 'getParameterHistory_name' - The name of the parameter for which you want to review history.
newGetParameterHistory ::
  -- | 'name'
  Prelude.Text ->
  GetParameterHistory
newGetParameterHistory pName_ =
  GetParameterHistory'
    { withDecryption =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      name = pName_
    }

-- | Return decrypted values for secure string parameters. This flag is
-- ignored for String and StringList parameter types.
getParameterHistory_withDecryption :: Lens.Lens' GetParameterHistory (Prelude.Maybe Prelude.Bool)
getParameterHistory_withDecryption = Lens.lens (\GetParameterHistory' {withDecryption} -> withDecryption) (\s@GetParameterHistory' {} a -> s {withDecryption = a} :: GetParameterHistory)

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
getParameterHistory_nextToken :: Lens.Lens' GetParameterHistory (Prelude.Maybe Prelude.Text)
getParameterHistory_nextToken = Lens.lens (\GetParameterHistory' {nextToken} -> nextToken) (\s@GetParameterHistory' {} a -> s {nextToken = a} :: GetParameterHistory)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
getParameterHistory_maxResults :: Lens.Lens' GetParameterHistory (Prelude.Maybe Prelude.Natural)
getParameterHistory_maxResults = Lens.lens (\GetParameterHistory' {maxResults} -> maxResults) (\s@GetParameterHistory' {} a -> s {maxResults = a} :: GetParameterHistory)

-- | The name of the parameter for which you want to review history.
getParameterHistory_name :: Lens.Lens' GetParameterHistory Prelude.Text
getParameterHistory_name = Lens.lens (\GetParameterHistory' {name} -> name) (\s@GetParameterHistory' {} a -> s {name = a} :: GetParameterHistory)

instance Pager.AWSPager GetParameterHistory where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? getParameterHistoryResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? getParameterHistoryResponse_parameters
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& getParameterHistory_nextToken
          Lens..~ rs
          Lens.^? getParameterHistoryResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest GetParameterHistory where
  type
    Rs GetParameterHistory =
      GetParameterHistoryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetParameterHistoryResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "Parameters"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetParameterHistory

instance Prelude.NFData GetParameterHistory

instance Prelude.ToHeaders GetParameterHistory where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonSSM.GetParameterHistory" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetParameterHistory where
  toJSON GetParameterHistory' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("WithDecryption" Prelude..=)
              Prelude.<$> withDecryption,
            ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            Prelude.Just ("Name" Prelude..= name)
          ]
      )

instance Prelude.ToPath GetParameterHistory where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetParameterHistory where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetParameterHistoryResponse' smart constructor.
data GetParameterHistoryResponse = GetParameterHistoryResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of parameters returned by the request.
    parameters :: Prelude.Maybe [ParameterHistory],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetParameterHistoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getParameterHistoryResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'parameters', 'getParameterHistoryResponse_parameters' - A list of parameters returned by the request.
--
-- 'httpStatus', 'getParameterHistoryResponse_httpStatus' - The response's http status code.
newGetParameterHistoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetParameterHistoryResponse
newGetParameterHistoryResponse pHttpStatus_ =
  GetParameterHistoryResponse'
    { nextToken =
        Prelude.Nothing,
      parameters = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
getParameterHistoryResponse_nextToken :: Lens.Lens' GetParameterHistoryResponse (Prelude.Maybe Prelude.Text)
getParameterHistoryResponse_nextToken = Lens.lens (\GetParameterHistoryResponse' {nextToken} -> nextToken) (\s@GetParameterHistoryResponse' {} a -> s {nextToken = a} :: GetParameterHistoryResponse)

-- | A list of parameters returned by the request.
getParameterHistoryResponse_parameters :: Lens.Lens' GetParameterHistoryResponse (Prelude.Maybe [ParameterHistory])
getParameterHistoryResponse_parameters = Lens.lens (\GetParameterHistoryResponse' {parameters} -> parameters) (\s@GetParameterHistoryResponse' {} a -> s {parameters = a} :: GetParameterHistoryResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
getParameterHistoryResponse_httpStatus :: Lens.Lens' GetParameterHistoryResponse Prelude.Int
getParameterHistoryResponse_httpStatus = Lens.lens (\GetParameterHistoryResponse' {httpStatus} -> httpStatus) (\s@GetParameterHistoryResponse' {} a -> s {httpStatus = a} :: GetParameterHistoryResponse)

instance Prelude.NFData GetParameterHistoryResponse
