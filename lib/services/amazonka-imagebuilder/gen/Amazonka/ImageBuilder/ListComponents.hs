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
    listComponents_byName,
    listComponents_filters,
    listComponents_maxResults,
    listComponents_nextToken,
    listComponents_owner,

    -- * Destructuring the Response
    ListComponentsResponse (..),
    newListComponentsResponse,

    -- * Response Lenses
    listComponentsResponse_componentVersionList,
    listComponentsResponse_nextToken,
    listComponentsResponse_requestId,
    listComponentsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListComponents' smart constructor.
data ListComponents = ListComponents'
  { -- | Returns the list of component build versions for the specified name.
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
    -- | The maximum items to return in a request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token to specify where to start paginating. This is the NextToken from
    -- a previously truncated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The owner defines which components you want to list. By default, this
    -- request will only show components owned by your account. You can use
    -- this field to specify if you want to view components owned by yourself,
    -- by Amazon, or those components that have been shared with you by other
    -- customers.
    owner :: Prelude.Maybe Ownership
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
-- 'maxResults', 'listComponents_maxResults' - The maximum items to return in a request.
--
-- 'nextToken', 'listComponents_nextToken' - A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
--
-- 'owner', 'listComponents_owner' - The owner defines which components you want to list. By default, this
-- request will only show components owned by your account. You can use
-- this field to specify if you want to view components owned by yourself,
-- by Amazon, or those components that have been shared with you by other
-- customers.
newListComponents ::
  ListComponents
newListComponents =
  ListComponents'
    { byName = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      owner = Prelude.Nothing
    }

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

-- | The maximum items to return in a request.
listComponents_maxResults :: Lens.Lens' ListComponents (Prelude.Maybe Prelude.Natural)
listComponents_maxResults = Lens.lens (\ListComponents' {maxResults} -> maxResults) (\s@ListComponents' {} a -> s {maxResults = a} :: ListComponents)

-- | A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
listComponents_nextToken :: Lens.Lens' ListComponents (Prelude.Maybe Prelude.Text)
listComponents_nextToken = Lens.lens (\ListComponents' {nextToken} -> nextToken) (\s@ListComponents' {} a -> s {nextToken = a} :: ListComponents)

-- | The owner defines which components you want to list. By default, this
-- request will only show components owned by your account. You can use
-- this field to specify if you want to view components owned by yourself,
-- by Amazon, or those components that have been shared with you by other
-- customers.
listComponents_owner :: Lens.Lens' ListComponents (Prelude.Maybe Ownership)
listComponents_owner = Lens.lens (\ListComponents' {owner} -> owner) (\s@ListComponents' {} a -> s {owner = a} :: ListComponents)

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
            Prelude.<$> ( x Data..?> "componentVersionList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "requestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListComponents where
  hashWithSalt _salt ListComponents' {..} =
    _salt `Prelude.hashWithSalt` byName
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` owner

instance Prelude.NFData ListComponents where
  rnf ListComponents' {..} =
    Prelude.rnf byName
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf owner

instance Data.ToHeaders ListComponents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListComponents where
  toJSON ListComponents' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("byName" Data..=) Prelude.<$> byName,
            ("filters" Data..=) Prelude.<$> filters,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("owner" Data..=) Prelude.<$> owner
          ]
      )

instance Data.ToPath ListComponents where
  toPath = Prelude.const "/ListComponents"

instance Data.ToQuery ListComponents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListComponentsResponse' smart constructor.
data ListComponentsResponse = ListComponentsResponse'
  { -- | The list of component semantic versions.
    --
    -- The semantic version has four nodes:
    -- \<major>.\<minor>.\<patch>\/\<build>. You can assign values for the
    -- first three, and can filter on all of them.
    componentVersionList :: Prelude.Maybe [ComponentVersion],
    -- | The next token used for paginated responses. When this is not empty,
    -- there are additional elements that the service has not included in this
    -- request. Use this token with the next request to retrieve additional
    -- objects.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'componentVersionList', 'listComponentsResponse_componentVersionList' - The list of component semantic versions.
--
-- The semantic version has four nodes:
-- \<major>.\<minor>.\<patch>\/\<build>. You can assign values for the
-- first three, and can filter on all of them.
--
-- 'nextToken', 'listComponentsResponse_nextToken' - The next token used for paginated responses. When this is not empty,
-- there are additional elements that the service has not included in this
-- request. Use this token with the next request to retrieve additional
-- objects.
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
    { componentVersionList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      requestId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of component semantic versions.
--
-- The semantic version has four nodes:
-- \<major>.\<minor>.\<patch>\/\<build>. You can assign values for the
-- first three, and can filter on all of them.
listComponentsResponse_componentVersionList :: Lens.Lens' ListComponentsResponse (Prelude.Maybe [ComponentVersion])
listComponentsResponse_componentVersionList = Lens.lens (\ListComponentsResponse' {componentVersionList} -> componentVersionList) (\s@ListComponentsResponse' {} a -> s {componentVersionList = a} :: ListComponentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The next token used for paginated responses. When this is not empty,
-- there are additional elements that the service has not included in this
-- request. Use this token with the next request to retrieve additional
-- objects.
listComponentsResponse_nextToken :: Lens.Lens' ListComponentsResponse (Prelude.Maybe Prelude.Text)
listComponentsResponse_nextToken = Lens.lens (\ListComponentsResponse' {nextToken} -> nextToken) (\s@ListComponentsResponse' {} a -> s {nextToken = a} :: ListComponentsResponse)

-- | The request ID that uniquely identifies this request.
listComponentsResponse_requestId :: Lens.Lens' ListComponentsResponse (Prelude.Maybe Prelude.Text)
listComponentsResponse_requestId = Lens.lens (\ListComponentsResponse' {requestId} -> requestId) (\s@ListComponentsResponse' {} a -> s {requestId = a} :: ListComponentsResponse)

-- | The response's http status code.
listComponentsResponse_httpStatus :: Lens.Lens' ListComponentsResponse Prelude.Int
listComponentsResponse_httpStatus = Lens.lens (\ListComponentsResponse' {httpStatus} -> httpStatus) (\s@ListComponentsResponse' {} a -> s {httpStatus = a} :: ListComponentsResponse)

instance Prelude.NFData ListComponentsResponse where
  rnf ListComponentsResponse' {..} =
    Prelude.rnf componentVersionList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf httpStatus
