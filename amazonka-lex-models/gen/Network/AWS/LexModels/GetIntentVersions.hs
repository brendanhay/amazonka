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
-- Module      : Network.AWS.LexModels.GetIntentVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about all of the versions of an intent.
--
-- The @GetIntentVersions@ operation returns an @IntentMetadata@ object for
-- each version of an intent. For example, if an intent has three numbered
-- versions, the @GetIntentVersions@ operation returns four
-- @IntentMetadata@ objects in the response, one for each numbered version
-- and one for the @$LATEST@ version.
--
-- The @GetIntentVersions@ operation always returns at least one version,
-- the @$LATEST@ version.
--
-- This operation requires permissions for the @lex:GetIntentVersions@
-- action.
--
-- This operation returns paginated results.
module Network.AWS.LexModels.GetIntentVersions
  ( -- * Creating a Request
    GetIntentVersions (..),
    newGetIntentVersions,

    -- * Request Lenses
    getIntentVersions_nextToken,
    getIntentVersions_maxResults,
    getIntentVersions_name,

    -- * Destructuring the Response
    GetIntentVersionsResponse (..),
    newGetIntentVersionsResponse,

    -- * Response Lenses
    getIntentVersionsResponse_nextToken,
    getIntentVersionsResponse_intents,
    getIntentVersionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetIntentVersions' smart constructor.
data GetIntentVersions = GetIntentVersions'
  { -- | A pagination token for fetching the next page of intent versions. If the
    -- response to this call is truncated, Amazon Lex returns a pagination
    -- token in the response. To fetch the next page of versions, specify the
    -- pagination token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of intent versions to return in the response. The
    -- default is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the intent for which versions should be returned.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetIntentVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getIntentVersions_nextToken' - A pagination token for fetching the next page of intent versions. If the
-- response to this call is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch the next page of versions, specify the
-- pagination token in the next request.
--
-- 'maxResults', 'getIntentVersions_maxResults' - The maximum number of intent versions to return in the response. The
-- default is 10.
--
-- 'name', 'getIntentVersions_name' - The name of the intent for which versions should be returned.
newGetIntentVersions ::
  -- | 'name'
  Prelude.Text ->
  GetIntentVersions
newGetIntentVersions pName_ =
  GetIntentVersions'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      name = pName_
    }

-- | A pagination token for fetching the next page of intent versions. If the
-- response to this call is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch the next page of versions, specify the
-- pagination token in the next request.
getIntentVersions_nextToken :: Lens.Lens' GetIntentVersions (Prelude.Maybe Prelude.Text)
getIntentVersions_nextToken = Lens.lens (\GetIntentVersions' {nextToken} -> nextToken) (\s@GetIntentVersions' {} a -> s {nextToken = a} :: GetIntentVersions)

-- | The maximum number of intent versions to return in the response. The
-- default is 10.
getIntentVersions_maxResults :: Lens.Lens' GetIntentVersions (Prelude.Maybe Prelude.Natural)
getIntentVersions_maxResults = Lens.lens (\GetIntentVersions' {maxResults} -> maxResults) (\s@GetIntentVersions' {} a -> s {maxResults = a} :: GetIntentVersions)

-- | The name of the intent for which versions should be returned.
getIntentVersions_name :: Lens.Lens' GetIntentVersions Prelude.Text
getIntentVersions_name = Lens.lens (\GetIntentVersions' {name} -> name) (\s@GetIntentVersions' {} a -> s {name = a} :: GetIntentVersions)

instance Pager.AWSPager GetIntentVersions where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? getIntentVersionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? getIntentVersionsResponse_intents
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& getIntentVersions_nextToken
          Lens..~ rs
          Lens.^? getIntentVersionsResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest GetIntentVersions where
  type Rs GetIntentVersions = GetIntentVersionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetIntentVersionsResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> (x Prelude..?> "intents" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetIntentVersions

instance Prelude.NFData GetIntentVersions

instance Prelude.ToHeaders GetIntentVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath GetIntentVersions where
  toPath GetIntentVersions' {..} =
    Prelude.mconcat
      ["/intents/", Prelude.toBS name, "/versions/"]

instance Prelude.ToQuery GetIntentVersions where
  toQuery GetIntentVersions' {..} =
    Prelude.mconcat
      [ "nextToken" Prelude.=: nextToken,
        "maxResults" Prelude.=: maxResults
      ]

-- | /See:/ 'newGetIntentVersionsResponse' smart constructor.
data GetIntentVersionsResponse = GetIntentVersionsResponse'
  { -- | A pagination token for fetching the next page of intent versions. If the
    -- response to this call is truncated, Amazon Lex returns a pagination
    -- token in the response. To fetch the next page of versions, specify the
    -- pagination token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of @IntentMetadata@ objects, one for each numbered version of
    -- the intent plus one for the @$LATEST@ version.
    intents :: Prelude.Maybe [IntentMetadata],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetIntentVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getIntentVersionsResponse_nextToken' - A pagination token for fetching the next page of intent versions. If the
-- response to this call is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch the next page of versions, specify the
-- pagination token in the next request.
--
-- 'intents', 'getIntentVersionsResponse_intents' - An array of @IntentMetadata@ objects, one for each numbered version of
-- the intent plus one for the @$LATEST@ version.
--
-- 'httpStatus', 'getIntentVersionsResponse_httpStatus' - The response's http status code.
newGetIntentVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetIntentVersionsResponse
newGetIntentVersionsResponse pHttpStatus_ =
  GetIntentVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      intents = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A pagination token for fetching the next page of intent versions. If the
-- response to this call is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch the next page of versions, specify the
-- pagination token in the next request.
getIntentVersionsResponse_nextToken :: Lens.Lens' GetIntentVersionsResponse (Prelude.Maybe Prelude.Text)
getIntentVersionsResponse_nextToken = Lens.lens (\GetIntentVersionsResponse' {nextToken} -> nextToken) (\s@GetIntentVersionsResponse' {} a -> s {nextToken = a} :: GetIntentVersionsResponse)

-- | An array of @IntentMetadata@ objects, one for each numbered version of
-- the intent plus one for the @$LATEST@ version.
getIntentVersionsResponse_intents :: Lens.Lens' GetIntentVersionsResponse (Prelude.Maybe [IntentMetadata])
getIntentVersionsResponse_intents = Lens.lens (\GetIntentVersionsResponse' {intents} -> intents) (\s@GetIntentVersionsResponse' {} a -> s {intents = a} :: GetIntentVersionsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
getIntentVersionsResponse_httpStatus :: Lens.Lens' GetIntentVersionsResponse Prelude.Int
getIntentVersionsResponse_httpStatus = Lens.lens (\GetIntentVersionsResponse' {httpStatus} -> httpStatus) (\s@GetIntentVersionsResponse' {} a -> s {httpStatus = a} :: GetIntentVersionsResponse)

instance Prelude.NFData GetIntentVersionsResponse
