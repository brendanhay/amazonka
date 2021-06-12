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
-- Module      : Network.AWS.MQ.ListConfigurations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all configurations.
module Network.AWS.MQ.ListConfigurations
  ( -- * Creating a Request
    ListConfigurations (..),
    newListConfigurations,

    -- * Request Lenses
    listConfigurations_nextToken,
    listConfigurations_maxResults,

    -- * Destructuring the Response
    ListConfigurationsResponse (..),
    newListConfigurationsResponse,

    -- * Response Lenses
    listConfigurationsResponse_nextToken,
    listConfigurationsResponse_maxResults,
    listConfigurationsResponse_configurations,
    listConfigurationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListConfigurations' smart constructor.
data ListConfigurations = ListConfigurations'
  { -- | The token that specifies the next page of results Amazon MQ should
    -- return. To request the first page, leave nextToken empty.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of configurations that Amazon MQ can return per page
    -- (20 by default). This value must be an integer from 5 to 100.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listConfigurations_nextToken' - The token that specifies the next page of results Amazon MQ should
-- return. To request the first page, leave nextToken empty.
--
-- 'maxResults', 'listConfigurations_maxResults' - The maximum number of configurations that Amazon MQ can return per page
-- (20 by default). This value must be an integer from 5 to 100.
newListConfigurations ::
  ListConfigurations
newListConfigurations =
  ListConfigurations'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The token that specifies the next page of results Amazon MQ should
-- return. To request the first page, leave nextToken empty.
listConfigurations_nextToken :: Lens.Lens' ListConfigurations (Core.Maybe Core.Text)
listConfigurations_nextToken = Lens.lens (\ListConfigurations' {nextToken} -> nextToken) (\s@ListConfigurations' {} a -> s {nextToken = a} :: ListConfigurations)

-- | The maximum number of configurations that Amazon MQ can return per page
-- (20 by default). This value must be an integer from 5 to 100.
listConfigurations_maxResults :: Lens.Lens' ListConfigurations (Core.Maybe Core.Natural)
listConfigurations_maxResults = Lens.lens (\ListConfigurations' {maxResults} -> maxResults) (\s@ListConfigurations' {} a -> s {maxResults = a} :: ListConfigurations)

instance Core.AWSRequest ListConfigurations where
  type
    AWSResponse ListConfigurations =
      ListConfigurationsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListConfigurationsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "maxResults")
            Core.<*> (x Core..?> "configurations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListConfigurations

instance Core.NFData ListConfigurations

instance Core.ToHeaders ListConfigurations where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListConfigurations where
  toPath = Core.const "/v1/configurations"

instance Core.ToQuery ListConfigurations where
  toQuery ListConfigurations' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListConfigurationsResponse' smart constructor.
data ListConfigurationsResponse = ListConfigurationsResponse'
  { -- | The token that specifies the next page of results Amazon MQ should
    -- return. To request the first page, leave nextToken empty.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of configurations that Amazon MQ can return per page
    -- (20 by default). This value must be an integer from 5 to 100.
    maxResults :: Core.Maybe Core.Int,
    -- | The list of all revisions for the specified configuration.
    configurations :: Core.Maybe [Configuration],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listConfigurationsResponse_nextToken' - The token that specifies the next page of results Amazon MQ should
-- return. To request the first page, leave nextToken empty.
--
-- 'maxResults', 'listConfigurationsResponse_maxResults' - The maximum number of configurations that Amazon MQ can return per page
-- (20 by default). This value must be an integer from 5 to 100.
--
-- 'configurations', 'listConfigurationsResponse_configurations' - The list of all revisions for the specified configuration.
--
-- 'httpStatus', 'listConfigurationsResponse_httpStatus' - The response's http status code.
newListConfigurationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListConfigurationsResponse
newListConfigurationsResponse pHttpStatus_ =
  ListConfigurationsResponse'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      configurations = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token that specifies the next page of results Amazon MQ should
-- return. To request the first page, leave nextToken empty.
listConfigurationsResponse_nextToken :: Lens.Lens' ListConfigurationsResponse (Core.Maybe Core.Text)
listConfigurationsResponse_nextToken = Lens.lens (\ListConfigurationsResponse' {nextToken} -> nextToken) (\s@ListConfigurationsResponse' {} a -> s {nextToken = a} :: ListConfigurationsResponse)

-- | The maximum number of configurations that Amazon MQ can return per page
-- (20 by default). This value must be an integer from 5 to 100.
listConfigurationsResponse_maxResults :: Lens.Lens' ListConfigurationsResponse (Core.Maybe Core.Int)
listConfigurationsResponse_maxResults = Lens.lens (\ListConfigurationsResponse' {maxResults} -> maxResults) (\s@ListConfigurationsResponse' {} a -> s {maxResults = a} :: ListConfigurationsResponse)

-- | The list of all revisions for the specified configuration.
listConfigurationsResponse_configurations :: Lens.Lens' ListConfigurationsResponse (Core.Maybe [Configuration])
listConfigurationsResponse_configurations = Lens.lens (\ListConfigurationsResponse' {configurations} -> configurations) (\s@ListConfigurationsResponse' {} a -> s {configurations = a} :: ListConfigurationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listConfigurationsResponse_httpStatus :: Lens.Lens' ListConfigurationsResponse Core.Int
listConfigurationsResponse_httpStatus = Lens.lens (\ListConfigurationsResponse' {httpStatus} -> httpStatus) (\s@ListConfigurationsResponse' {} a -> s {httpStatus = a} :: ListConfigurationsResponse)

instance Core.NFData ListConfigurationsResponse
