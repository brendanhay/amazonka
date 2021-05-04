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
-- Module      : Network.AWS.AppSync.ListResolvers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the resolvers for a given API and type.
--
-- This operation returns paginated results.
module Network.AWS.AppSync.ListResolvers
  ( -- * Creating a Request
    ListResolvers (..),
    newListResolvers,

    -- * Request Lenses
    listResolvers_nextToken,
    listResolvers_maxResults,
    listResolvers_apiId,
    listResolvers_typeName,

    -- * Destructuring the Response
    ListResolversResponse (..),
    newListResolversResponse,

    -- * Response Lenses
    listResolversResponse_nextToken,
    listResolversResponse_resolvers,
    listResolversResponse_httpStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListResolvers' smart constructor.
data ListResolvers = ListResolvers'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results you want the request to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The API ID.
    apiId :: Prelude.Text,
    -- | The type name.
    typeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListResolvers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResolvers_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'maxResults', 'listResolvers_maxResults' - The maximum number of results you want the request to return.
--
-- 'apiId', 'listResolvers_apiId' - The API ID.
--
-- 'typeName', 'listResolvers_typeName' - The type name.
newListResolvers ::
  -- | 'apiId'
  Prelude.Text ->
  -- | 'typeName'
  Prelude.Text ->
  ListResolvers
newListResolvers pApiId_ pTypeName_ =
  ListResolvers'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      apiId = pApiId_,
      typeName = pTypeName_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listResolvers_nextToken :: Lens.Lens' ListResolvers (Prelude.Maybe Prelude.Text)
listResolvers_nextToken = Lens.lens (\ListResolvers' {nextToken} -> nextToken) (\s@ListResolvers' {} a -> s {nextToken = a} :: ListResolvers)

-- | The maximum number of results you want the request to return.
listResolvers_maxResults :: Lens.Lens' ListResolvers (Prelude.Maybe Prelude.Natural)
listResolvers_maxResults = Lens.lens (\ListResolvers' {maxResults} -> maxResults) (\s@ListResolvers' {} a -> s {maxResults = a} :: ListResolvers)

-- | The API ID.
listResolvers_apiId :: Lens.Lens' ListResolvers Prelude.Text
listResolvers_apiId = Lens.lens (\ListResolvers' {apiId} -> apiId) (\s@ListResolvers' {} a -> s {apiId = a} :: ListResolvers)

-- | The type name.
listResolvers_typeName :: Lens.Lens' ListResolvers Prelude.Text
listResolvers_typeName = Lens.lens (\ListResolvers' {typeName} -> typeName) (\s@ListResolvers' {} a -> s {typeName = a} :: ListResolvers)

instance Pager.AWSPager ListResolvers where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listResolversResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listResolversResponse_resolvers Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listResolvers_nextToken
          Lens..~ rs
          Lens.^? listResolversResponse_nextToken Prelude.. Lens._Just

instance Prelude.AWSRequest ListResolvers where
  type Rs ListResolvers = ListResolversResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResolversResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> ( x Prelude..?> "resolvers"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListResolvers

instance Prelude.NFData ListResolvers

instance Prelude.ToHeaders ListResolvers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath ListResolvers where
  toPath ListResolvers' {..} =
    Prelude.mconcat
      [ "/v1/apis/",
        Prelude.toBS apiId,
        "/types/",
        Prelude.toBS typeName,
        "/resolvers"
      ]

instance Prelude.ToQuery ListResolvers where
  toQuery ListResolvers' {..} =
    Prelude.mconcat
      [ "nextToken" Prelude.=: nextToken,
        "maxResults" Prelude.=: maxResults
      ]

-- | /See:/ 'newListResolversResponse' smart constructor.
data ListResolversResponse = ListResolversResponse'
  { -- | An identifier to be passed in the next request to this operation to
    -- return the next set of items in the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The @Resolver@ objects.
    resolvers :: Prelude.Maybe [Resolver],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListResolversResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResolversResponse_nextToken' - An identifier to be passed in the next request to this operation to
-- return the next set of items in the list.
--
-- 'resolvers', 'listResolversResponse_resolvers' - The @Resolver@ objects.
--
-- 'httpStatus', 'listResolversResponse_httpStatus' - The response's http status code.
newListResolversResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListResolversResponse
newListResolversResponse pHttpStatus_ =
  ListResolversResponse'
    { nextToken = Prelude.Nothing,
      resolvers = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An identifier to be passed in the next request to this operation to
-- return the next set of items in the list.
listResolversResponse_nextToken :: Lens.Lens' ListResolversResponse (Prelude.Maybe Prelude.Text)
listResolversResponse_nextToken = Lens.lens (\ListResolversResponse' {nextToken} -> nextToken) (\s@ListResolversResponse' {} a -> s {nextToken = a} :: ListResolversResponse)

-- | The @Resolver@ objects.
listResolversResponse_resolvers :: Lens.Lens' ListResolversResponse (Prelude.Maybe [Resolver])
listResolversResponse_resolvers = Lens.lens (\ListResolversResponse' {resolvers} -> resolvers) (\s@ListResolversResponse' {} a -> s {resolvers = a} :: ListResolversResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listResolversResponse_httpStatus :: Lens.Lens' ListResolversResponse Prelude.Int
listResolversResponse_httpStatus = Lens.lens (\ListResolversResponse' {httpStatus} -> httpStatus) (\s@ListResolversResponse' {} a -> s {httpStatus = a} :: ListResolversResponse)

instance Prelude.NFData ListResolversResponse
