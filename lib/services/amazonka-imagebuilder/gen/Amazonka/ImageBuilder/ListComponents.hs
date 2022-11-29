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
-- Module      : Amazonka.ImageBuilder.ListComponents
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of component build versions for the specified semantic
-- version.
--
-- The semantic version has four nodes:
-- \<major>.\<minor>.\<patch>\/\<build>. You can assign values for the
-- first three, and can filter on all of them.
--
-- __Filtering:__ With semantic versioning, you have the flexibility to use
-- wildcards (x) to specify the most recent versions or nodes when
-- selecting the base image or components for your recipe. When you use a
-- wildcard in any node, all nodes to the right of the first wildcard must
-- also be wildcards.
module Amazonka.ImageBuilder.ListComponents
  ( -- * Creating a Request
    ListComponents (..),
    newListComponents,

    -- * Request Lenses
    listComponents_nextToken,
    listComponents_byName,
    listComponents_filters,
    listComponents_owner,
    listComponents_maxResults,

    -- * Destructuring the Response
    ListComponentsResponse (..),
    newListComponentsResponse,

    -- * Response Lenses
    listComponentsResponse_nextToken,
    listComponentsResponse_componentVersionList,
    listComponentsResponse_requestId,
    listComponentsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListComponents' smart constructor.
data ListComponents = ListComponents'
  { -- | A token to specify where to start paginating. This is the NextToken from
    -- a previously truncated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns the list of component build versions for the specified name.
    byName :: Prelude.Maybe Prelude.Bool,
    -- | Use the following filters to streamline results:
    --
    -- -   @description@
    --
    -- -   @name@
    --
    -- -   @platform@
    --
    -- -   @supportedOsVersion@
    --
    -- -   @type@
    --
    -- -   @version@
    filters :: Prelude.Maybe (Prelude.NonEmpty Filter),
    -- | The owner defines which components you want to list. By default, this
    -- request will only show components owned by your account. You can use
    -- this field to specify if you want to view components owned by yourself,
    -- by Amazon, or those components that have been shared with you by other
    -- customers.
    owner :: Prelude.Maybe Ownership,
    -- | The maximum items to return in a request.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListComponents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listComponents_nextToken' - A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
--
-- 'byName', 'listComponents_byName' - Returns the list of component build versions for the specified name.
--
-- 'filters', 'listComponents_filters' - Use the following filters to streamline results:
--
-- -   @description@
--
-- -   @name@
--
-- -   @platform@
--
-- -   @supportedOsVersion@
--
-- -   @type@
--
-- -   @version@
--
-- 'owner', 'listComponents_owner' - The owner defines which components you want to list. By default, this
-- request will only show components owned by your account. You can use
-- this field to specify if you want to view components owned by yourself,
-- by Amazon, or those components that have been shared with you by other
-- customers.
--
-- 'maxResults', 'listComponents_maxResults' - The maximum items to return in a request.
newListComponents ::
  ListComponents
newListComponents =
  ListComponents'
    { nextToken = Prelude.Nothing,
      byName = Prelude.Nothing,
      filters = Prelude.Nothing,
      owner = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
listComponents_nextToken :: Lens.Lens' ListComponents (Prelude.Maybe Prelude.Text)
listComponents_nextToken = Lens.lens (\ListComponents' {nextToken} -> nextToken) (\s@ListComponents' {} a -> s {nextToken = a} :: ListComponents)

-- | Returns the list of component build versions for the specified name.
listComponents_byName :: Lens.Lens' ListComponents (Prelude.Maybe Prelude.Bool)
listComponents_byName = Lens.lens (\ListComponents' {byName} -> byName) (\s@ListComponents' {} a -> s {byName = a} :: ListComponents)

-- | Use the following filters to streamline results:
--
-- -   @description@
--
-- -   @name@
--
-- -   @platform@
--
-- -   @supportedOsVersion@
--
-- -   @type@
--
-- -   @version@
listComponents_filters :: Lens.Lens' ListComponents (Prelude.Maybe (Prelude.NonEmpty Filter))
listComponents_filters = Lens.lens (\ListComponents' {filters} -> filters) (\s@ListComponents' {} a -> s {filters = a} :: ListComponents) Prelude.. Lens.mapping Lens.coerced

-- | The owner defines which components you want to list. By default, this
-- request will only show components owned by your account. You can use
-- this field to specify if you want to view components owned by yourself,
-- by Amazon, or those components that have been shared with you by other
-- customers.
listComponents_owner :: Lens.Lens' ListComponents (Prelude.Maybe Ownership)
listComponents_owner = Lens.lens (\ListComponents' {owner} -> owner) (\s@ListComponents' {} a -> s {owner = a} :: ListComponents)

-- | The maximum items to return in a request.
listComponents_maxResults :: Lens.Lens' ListComponents (Prelude.Maybe Prelude.Natural)
listComponents_maxResults = Lens.lens (\ListComponents' {maxResults} -> maxResults) (\s@ListComponents' {} a -> s {maxResults = a} :: ListComponents)

instance Core.AWSRequest ListComponents where
  type
    AWSResponse ListComponents =
      ListComponentsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListComponentsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "componentVersionList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "requestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListComponents where
  hashWithSalt _salt ListComponents' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` byName
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListComponents where
  rnf ListComponents' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf byName
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListComponents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListComponents where
  toJSON ListComponents' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("byName" Core..=) Prelude.<$> byName,
            ("filters" Core..=) Prelude.<$> filters,
            ("owner" Core..=) Prelude.<$> owner,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListComponents where
  toPath = Prelude.const "/ListComponents"

instance Core.ToQuery ListComponents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListComponentsResponse' smart constructor.
data ListComponentsResponse = ListComponentsResponse'
  { -- | The next token used for paginated responses. When this is not empty,
    -- there are additional elements that the service has not included in this
    -- request. Use this token with the next request to retrieve additional
    -- objects.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of component semantic versions.
    --
    -- The semantic version has four nodes:
    -- \<major>.\<minor>.\<patch>\/\<build>. You can assign values for the
    -- first three, and can filter on all of them.
    componentVersionList :: Prelude.Maybe [ComponentVersion],
    -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListComponentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listComponentsResponse_nextToken' - The next token used for paginated responses. When this is not empty,
-- there are additional elements that the service has not included in this
-- request. Use this token with the next request to retrieve additional
-- objects.
--
-- 'componentVersionList', 'listComponentsResponse_componentVersionList' - The list of component semantic versions.
--
-- The semantic version has four nodes:
-- \<major>.\<minor>.\<patch>\/\<build>. You can assign values for the
-- first three, and can filter on all of them.
--
-- 'requestId', 'listComponentsResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'httpStatus', 'listComponentsResponse_httpStatus' - The response's http status code.
newListComponentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListComponentsResponse
newListComponentsResponse pHttpStatus_ =
  ListComponentsResponse'
    { nextToken =
        Prelude.Nothing,
      componentVersionList = Prelude.Nothing,
      requestId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The next token used for paginated responses. When this is not empty,
-- there are additional elements that the service has not included in this
-- request. Use this token with the next request to retrieve additional
-- objects.
listComponentsResponse_nextToken :: Lens.Lens' ListComponentsResponse (Prelude.Maybe Prelude.Text)
listComponentsResponse_nextToken = Lens.lens (\ListComponentsResponse' {nextToken} -> nextToken) (\s@ListComponentsResponse' {} a -> s {nextToken = a} :: ListComponentsResponse)

-- | The list of component semantic versions.
--
-- The semantic version has four nodes:
-- \<major>.\<minor>.\<patch>\/\<build>. You can assign values for the
-- first three, and can filter on all of them.
listComponentsResponse_componentVersionList :: Lens.Lens' ListComponentsResponse (Prelude.Maybe [ComponentVersion])
listComponentsResponse_componentVersionList = Lens.lens (\ListComponentsResponse' {componentVersionList} -> componentVersionList) (\s@ListComponentsResponse' {} a -> s {componentVersionList = a} :: ListComponentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The request ID that uniquely identifies this request.
listComponentsResponse_requestId :: Lens.Lens' ListComponentsResponse (Prelude.Maybe Prelude.Text)
listComponentsResponse_requestId = Lens.lens (\ListComponentsResponse' {requestId} -> requestId) (\s@ListComponentsResponse' {} a -> s {requestId = a} :: ListComponentsResponse)

-- | The response's http status code.
listComponentsResponse_httpStatus :: Lens.Lens' ListComponentsResponse Prelude.Int
listComponentsResponse_httpStatus = Lens.lens (\ListComponentsResponse' {httpStatus} -> httpStatus) (\s@ListComponentsResponse' {} a -> s {httpStatus = a} :: ListComponentsResponse)

instance Prelude.NFData ListComponentsResponse where
  rnf ListComponentsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf componentVersionList
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf httpStatus
