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
-- Module      : Amazonka.Glue.ListDevEndpoints
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the names of all @DevEndpoint@ resources in this Amazon Web
-- Services account, or the resources with the specified tag. This
-- operation allows you to see which resources are available in your
-- account, and their names.
--
-- This operation takes the optional @Tags@ field, which you can use as a
-- filter on the response so that tagged resources can be retrieved as a
-- group. If you choose to use tags filtering, only resources with the tag
-- are retrieved.
module Amazonka.Glue.ListDevEndpoints
  ( -- * Creating a Request
    ListDevEndpoints (..),
    newListDevEndpoints,

    -- * Request Lenses
    listDevEndpoints_maxResults,
    listDevEndpoints_nextToken,
    listDevEndpoints_tags,

    -- * Destructuring the Response
    ListDevEndpointsResponse (..),
    newListDevEndpointsResponse,

    -- * Response Lenses
    listDevEndpointsResponse_devEndpointNames,
    listDevEndpointsResponse_nextToken,
    listDevEndpointsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDevEndpoints' smart constructor.
data ListDevEndpoints = ListDevEndpoints'
  { -- | The maximum size of a list to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A continuation token, if this is a continuation request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies to return only these tagged resources.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDevEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listDevEndpoints_maxResults' - The maximum size of a list to return.
--
-- 'nextToken', 'listDevEndpoints_nextToken' - A continuation token, if this is a continuation request.
--
-- 'tags', 'listDevEndpoints_tags' - Specifies to return only these tagged resources.
newListDevEndpoints ::
  ListDevEndpoints
newListDevEndpoints =
  ListDevEndpoints'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The maximum size of a list to return.
listDevEndpoints_maxResults :: Lens.Lens' ListDevEndpoints (Prelude.Maybe Prelude.Natural)
listDevEndpoints_maxResults = Lens.lens (\ListDevEndpoints' {maxResults} -> maxResults) (\s@ListDevEndpoints' {} a -> s {maxResults = a} :: ListDevEndpoints)

-- | A continuation token, if this is a continuation request.
listDevEndpoints_nextToken :: Lens.Lens' ListDevEndpoints (Prelude.Maybe Prelude.Text)
listDevEndpoints_nextToken = Lens.lens (\ListDevEndpoints' {nextToken} -> nextToken) (\s@ListDevEndpoints' {} a -> s {nextToken = a} :: ListDevEndpoints)

-- | Specifies to return only these tagged resources.
listDevEndpoints_tags :: Lens.Lens' ListDevEndpoints (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
listDevEndpoints_tags = Lens.lens (\ListDevEndpoints' {tags} -> tags) (\s@ListDevEndpoints' {} a -> s {tags = a} :: ListDevEndpoints) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest ListDevEndpoints where
  type
    AWSResponse ListDevEndpoints =
      ListDevEndpointsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDevEndpointsResponse'
            Prelude.<$> ( x Data..?> "DevEndpointNames"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDevEndpoints where
  hashWithSalt _salt ListDevEndpoints' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` tags

instance Prelude.NFData ListDevEndpoints where
  rnf ListDevEndpoints' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf tags

instance Data.ToHeaders ListDevEndpoints where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.ListDevEndpoints" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDevEndpoints where
  toJSON ListDevEndpoints' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Tags" Data..=) Prelude.<$> tags
          ]
      )

instance Data.ToPath ListDevEndpoints where
  toPath = Prelude.const "/"

instance Data.ToQuery ListDevEndpoints where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDevEndpointsResponse' smart constructor.
data ListDevEndpointsResponse = ListDevEndpointsResponse'
  { -- | The names of all the @DevEndpoint@s in the account, or the
    -- @DevEndpoint@s with the specified tags.
    devEndpointNames :: Prelude.Maybe [Prelude.Text],
    -- | A continuation token, if the returned list does not contain the last
    -- metric available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDevEndpointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'devEndpointNames', 'listDevEndpointsResponse_devEndpointNames' - The names of all the @DevEndpoint@s in the account, or the
-- @DevEndpoint@s with the specified tags.
--
-- 'nextToken', 'listDevEndpointsResponse_nextToken' - A continuation token, if the returned list does not contain the last
-- metric available.
--
-- 'httpStatus', 'listDevEndpointsResponse_httpStatus' - The response's http status code.
newListDevEndpointsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDevEndpointsResponse
newListDevEndpointsResponse pHttpStatus_ =
  ListDevEndpointsResponse'
    { devEndpointNames =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The names of all the @DevEndpoint@s in the account, or the
-- @DevEndpoint@s with the specified tags.
listDevEndpointsResponse_devEndpointNames :: Lens.Lens' ListDevEndpointsResponse (Prelude.Maybe [Prelude.Text])
listDevEndpointsResponse_devEndpointNames = Lens.lens (\ListDevEndpointsResponse' {devEndpointNames} -> devEndpointNames) (\s@ListDevEndpointsResponse' {} a -> s {devEndpointNames = a} :: ListDevEndpointsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A continuation token, if the returned list does not contain the last
-- metric available.
listDevEndpointsResponse_nextToken :: Lens.Lens' ListDevEndpointsResponse (Prelude.Maybe Prelude.Text)
listDevEndpointsResponse_nextToken = Lens.lens (\ListDevEndpointsResponse' {nextToken} -> nextToken) (\s@ListDevEndpointsResponse' {} a -> s {nextToken = a} :: ListDevEndpointsResponse)

-- | The response's http status code.
listDevEndpointsResponse_httpStatus :: Lens.Lens' ListDevEndpointsResponse Prelude.Int
listDevEndpointsResponse_httpStatus = Lens.lens (\ListDevEndpointsResponse' {httpStatus} -> httpStatus) (\s@ListDevEndpointsResponse' {} a -> s {httpStatus = a} :: ListDevEndpointsResponse)

instance Prelude.NFData ListDevEndpointsResponse where
  rnf ListDevEndpointsResponse' {..} =
    Prelude.rnf devEndpointNames
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
