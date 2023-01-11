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
-- Module      : Amazonka.SSM.GetParameterHistory
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the history of all changes to a parameter.
--
-- If you change the KMS key alias for the KMS key used to encrypt a
-- parameter, then you must also update the key alias the parameter uses to
-- reference KMS. Otherwise, @GetParameterHistory@ retrieves whatever the
-- original key alias was referencing.
--
-- This operation returns paginated results.
module Amazonka.SSM.GetParameterHistory
  ( -- * Creating a Request
    GetParameterHistory (..),
    newGetParameterHistory,

    -- * Request Lenses
    getParameterHistory_maxResults,
    getParameterHistory_nextToken,
    getParameterHistory_withDecryption,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newGetParameterHistory' smart constructor.
data GetParameterHistory = GetParameterHistory'
  { -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Return decrypted values for secure string parameters. This flag is
    -- ignored for @String@ and @StringList@ parameter types.
    withDecryption :: Prelude.Maybe Prelude.Bool,
    -- | The name of the parameter for which you want to review history.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetParameterHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getParameterHistory_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'nextToken', 'getParameterHistory_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'withDecryption', 'getParameterHistory_withDecryption' - Return decrypted values for secure string parameters. This flag is
-- ignored for @String@ and @StringList@ parameter types.
--
-- 'name', 'getParameterHistory_name' - The name of the parameter for which you want to review history.
newGetParameterHistory ::
  -- | 'name'
  Prelude.Text ->
  GetParameterHistory
newGetParameterHistory pName_ =
  GetParameterHistory'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      withDecryption = Prelude.Nothing,
      name = pName_
    }

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
getParameterHistory_maxResults :: Lens.Lens' GetParameterHistory (Prelude.Maybe Prelude.Natural)
getParameterHistory_maxResults = Lens.lens (\GetParameterHistory' {maxResults} -> maxResults) (\s@GetParameterHistory' {} a -> s {maxResults = a} :: GetParameterHistory)

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
getParameterHistory_nextToken :: Lens.Lens' GetParameterHistory (Prelude.Maybe Prelude.Text)
getParameterHistory_nextToken = Lens.lens (\GetParameterHistory' {nextToken} -> nextToken) (\s@GetParameterHistory' {} a -> s {nextToken = a} :: GetParameterHistory)

-- | Return decrypted values for secure string parameters. This flag is
-- ignored for @String@ and @StringList@ parameter types.
getParameterHistory_withDecryption :: Lens.Lens' GetParameterHistory (Prelude.Maybe Prelude.Bool)
getParameterHistory_withDecryption = Lens.lens (\GetParameterHistory' {withDecryption} -> withDecryption) (\s@GetParameterHistory' {} a -> s {withDecryption = a} :: GetParameterHistory)

-- | The name of the parameter for which you want to review history.
getParameterHistory_name :: Lens.Lens' GetParameterHistory Prelude.Text
getParameterHistory_name = Lens.lens (\GetParameterHistory' {name} -> name) (\s@GetParameterHistory' {} a -> s {name = a} :: GetParameterHistory)

instance Core.AWSPager GetParameterHistory where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getParameterHistoryResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getParameterHistoryResponse_parameters
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getParameterHistory_nextToken
          Lens..~ rs
          Lens.^? getParameterHistoryResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetParameterHistory where
  type
    AWSResponse GetParameterHistory =
      GetParameterHistoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetParameterHistoryResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Parameters" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetParameterHistory where
  hashWithSalt _salt GetParameterHistory' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` withDecryption
      `Prelude.hashWithSalt` name

instance Prelude.NFData GetParameterHistory where
  rnf GetParameterHistory' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf withDecryption
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders GetParameterHistory where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.GetParameterHistory" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetParameterHistory where
  toJSON GetParameterHistory' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("WithDecryption" Data..=)
              Prelude.<$> withDecryption,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath GetParameterHistory where
  toPath = Prelude.const "/"

instance Data.ToQuery GetParameterHistory where
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
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
getParameterHistoryResponse_parameters = Lens.lens (\GetParameterHistoryResponse' {parameters} -> parameters) (\s@GetParameterHistoryResponse' {} a -> s {parameters = a} :: GetParameterHistoryResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getParameterHistoryResponse_httpStatus :: Lens.Lens' GetParameterHistoryResponse Prelude.Int
getParameterHistoryResponse_httpStatus = Lens.lens (\GetParameterHistoryResponse' {httpStatus} -> httpStatus) (\s@GetParameterHistoryResponse' {} a -> s {httpStatus = a} :: GetParameterHistoryResponse)

instance Prelude.NFData GetParameterHistoryResponse where
  rnf GetParameterHistoryResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf httpStatus
