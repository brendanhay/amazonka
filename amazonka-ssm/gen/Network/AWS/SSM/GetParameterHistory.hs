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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newGetParameterHistory' smart constructor.
data GetParameterHistory = GetParameterHistory'
  { -- | Return decrypted values for secure string parameters. This flag is
    -- ignored for String and StringList parameter types.
    withDecryption :: Core.Maybe Core.Bool,
    -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The name of the parameter for which you want to review history.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GetParameterHistory
newGetParameterHistory pName_ =
  GetParameterHistory'
    { withDecryption = Core.Nothing,
      nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      name = pName_
    }

-- | Return decrypted values for secure string parameters. This flag is
-- ignored for String and StringList parameter types.
getParameterHistory_withDecryption :: Lens.Lens' GetParameterHistory (Core.Maybe Core.Bool)
getParameterHistory_withDecryption = Lens.lens (\GetParameterHistory' {withDecryption} -> withDecryption) (\s@GetParameterHistory' {} a -> s {withDecryption = a} :: GetParameterHistory)

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
getParameterHistory_nextToken :: Lens.Lens' GetParameterHistory (Core.Maybe Core.Text)
getParameterHistory_nextToken = Lens.lens (\GetParameterHistory' {nextToken} -> nextToken) (\s@GetParameterHistory' {} a -> s {nextToken = a} :: GetParameterHistory)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
getParameterHistory_maxResults :: Lens.Lens' GetParameterHistory (Core.Maybe Core.Natural)
getParameterHistory_maxResults = Lens.lens (\GetParameterHistory' {maxResults} -> maxResults) (\s@GetParameterHistory' {} a -> s {maxResults = a} :: GetParameterHistory)

-- | The name of the parameter for which you want to review history.
getParameterHistory_name :: Lens.Lens' GetParameterHistory Core.Text
getParameterHistory_name = Lens.lens (\GetParameterHistory' {name} -> name) (\s@GetParameterHistory' {} a -> s {name = a} :: GetParameterHistory)

instance Core.AWSPager GetParameterHistory where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getParameterHistoryResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getParameterHistoryResponse_parameters
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getParameterHistory_nextToken
          Lens..~ rs
          Lens.^? getParameterHistoryResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest GetParameterHistory where
  type
    AWSResponse GetParameterHistory =
      GetParameterHistoryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetParameterHistoryResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Parameters" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetParameterHistory

instance Core.NFData GetParameterHistory

instance Core.ToHeaders GetParameterHistory where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.GetParameterHistory" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetParameterHistory where
  toJSON GetParameterHistory' {..} =
    Core.object
      ( Core.catMaybes
          [ ("WithDecryption" Core..=) Core.<$> withDecryption,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath GetParameterHistory where
  toPath = Core.const "/"

instance Core.ToQuery GetParameterHistory where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetParameterHistoryResponse' smart constructor.
data GetParameterHistoryResponse = GetParameterHistoryResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of parameters returned by the request.
    parameters :: Core.Maybe [ParameterHistory],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetParameterHistoryResponse
newGetParameterHistoryResponse pHttpStatus_ =
  GetParameterHistoryResponse'
    { nextToken =
        Core.Nothing,
      parameters = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
getParameterHistoryResponse_nextToken :: Lens.Lens' GetParameterHistoryResponse (Core.Maybe Core.Text)
getParameterHistoryResponse_nextToken = Lens.lens (\GetParameterHistoryResponse' {nextToken} -> nextToken) (\s@GetParameterHistoryResponse' {} a -> s {nextToken = a} :: GetParameterHistoryResponse)

-- | A list of parameters returned by the request.
getParameterHistoryResponse_parameters :: Lens.Lens' GetParameterHistoryResponse (Core.Maybe [ParameterHistory])
getParameterHistoryResponse_parameters = Lens.lens (\GetParameterHistoryResponse' {parameters} -> parameters) (\s@GetParameterHistoryResponse' {} a -> s {parameters = a} :: GetParameterHistoryResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getParameterHistoryResponse_httpStatus :: Lens.Lens' GetParameterHistoryResponse Core.Int
getParameterHistoryResponse_httpStatus = Lens.lens (\GetParameterHistoryResponse' {httpStatus} -> httpStatus) (\s@GetParameterHistoryResponse' {} a -> s {httpStatus = a} :: GetParameterHistoryResponse)

instance Core.NFData GetParameterHistoryResponse
