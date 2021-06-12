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
-- Module      : Network.AWS.ElasticSearch.GetCompatibleElasticsearchVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of upgrade compatible Elastisearch versions. You can
-- optionally pass a @ DomainName @ to get all upgrade compatible
-- Elasticsearch versions for that specific domain.
module Network.AWS.ElasticSearch.GetCompatibleElasticsearchVersions
  ( -- * Creating a Request
    GetCompatibleElasticsearchVersions (..),
    newGetCompatibleElasticsearchVersions,

    -- * Request Lenses
    getCompatibleElasticsearchVersions_domainName,

    -- * Destructuring the Response
    GetCompatibleElasticsearchVersionsResponse (..),
    newGetCompatibleElasticsearchVersionsResponse,

    -- * Response Lenses
    getCompatibleElasticsearchVersionsResponse_compatibleElasticsearchVersions,
    getCompatibleElasticsearchVersionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for request parameters to
-- @ GetCompatibleElasticsearchVersions @ operation.
--
-- /See:/ 'newGetCompatibleElasticsearchVersions' smart constructor.
data GetCompatibleElasticsearchVersions = GetCompatibleElasticsearchVersions'
  { domainName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetCompatibleElasticsearchVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'getCompatibleElasticsearchVersions_domainName' - Undocumented member.
newGetCompatibleElasticsearchVersions ::
  GetCompatibleElasticsearchVersions
newGetCompatibleElasticsearchVersions =
  GetCompatibleElasticsearchVersions'
    { domainName =
        Core.Nothing
    }

-- | Undocumented member.
getCompatibleElasticsearchVersions_domainName :: Lens.Lens' GetCompatibleElasticsearchVersions (Core.Maybe Core.Text)
getCompatibleElasticsearchVersions_domainName = Lens.lens (\GetCompatibleElasticsearchVersions' {domainName} -> domainName) (\s@GetCompatibleElasticsearchVersions' {} a -> s {domainName = a} :: GetCompatibleElasticsearchVersions)

instance
  Core.AWSRequest
    GetCompatibleElasticsearchVersions
  where
  type
    AWSResponse GetCompatibleElasticsearchVersions =
      GetCompatibleElasticsearchVersionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCompatibleElasticsearchVersionsResponse'
            Core.<$> ( x Core..?> "CompatibleElasticsearchVersions"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    GetCompatibleElasticsearchVersions

instance
  Core.NFData
    GetCompatibleElasticsearchVersions

instance
  Core.ToHeaders
    GetCompatibleElasticsearchVersions
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    GetCompatibleElasticsearchVersions
  where
  toPath =
    Core.const "/2015-01-01/es/compatibleVersions"

instance
  Core.ToQuery
    GetCompatibleElasticsearchVersions
  where
  toQuery GetCompatibleElasticsearchVersions' {..} =
    Core.mconcat ["domainName" Core.=: domainName]

-- | Container for response returned by
-- @ GetCompatibleElasticsearchVersions @ operation.
--
-- /See:/ 'newGetCompatibleElasticsearchVersionsResponse' smart constructor.
data GetCompatibleElasticsearchVersionsResponse = GetCompatibleElasticsearchVersionsResponse'
  { -- | A map of compatible Elasticsearch versions returned as part of the
    -- @ GetCompatibleElasticsearchVersions @ operation.
    compatibleElasticsearchVersions :: Core.Maybe [CompatibleVersionsMap],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetCompatibleElasticsearchVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compatibleElasticsearchVersions', 'getCompatibleElasticsearchVersionsResponse_compatibleElasticsearchVersions' - A map of compatible Elasticsearch versions returned as part of the
-- @ GetCompatibleElasticsearchVersions @ operation.
--
-- 'httpStatus', 'getCompatibleElasticsearchVersionsResponse_httpStatus' - The response's http status code.
newGetCompatibleElasticsearchVersionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetCompatibleElasticsearchVersionsResponse
newGetCompatibleElasticsearchVersionsResponse
  pHttpStatus_ =
    GetCompatibleElasticsearchVersionsResponse'
      { compatibleElasticsearchVersions =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A map of compatible Elasticsearch versions returned as part of the
-- @ GetCompatibleElasticsearchVersions @ operation.
getCompatibleElasticsearchVersionsResponse_compatibleElasticsearchVersions :: Lens.Lens' GetCompatibleElasticsearchVersionsResponse (Core.Maybe [CompatibleVersionsMap])
getCompatibleElasticsearchVersionsResponse_compatibleElasticsearchVersions = Lens.lens (\GetCompatibleElasticsearchVersionsResponse' {compatibleElasticsearchVersions} -> compatibleElasticsearchVersions) (\s@GetCompatibleElasticsearchVersionsResponse' {} a -> s {compatibleElasticsearchVersions = a} :: GetCompatibleElasticsearchVersionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getCompatibleElasticsearchVersionsResponse_httpStatus :: Lens.Lens' GetCompatibleElasticsearchVersionsResponse Core.Int
getCompatibleElasticsearchVersionsResponse_httpStatus = Lens.lens (\GetCompatibleElasticsearchVersionsResponse' {httpStatus} -> httpStatus) (\s@GetCompatibleElasticsearchVersionsResponse' {} a -> s {httpStatus = a} :: GetCompatibleElasticsearchVersionsResponse)

instance
  Core.NFData
    GetCompatibleElasticsearchVersionsResponse
