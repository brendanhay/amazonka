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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for request parameters to
-- @ GetCompatibleElasticsearchVersions @ operation.
--
-- /See:/ 'newGetCompatibleElasticsearchVersions' smart constructor.
data GetCompatibleElasticsearchVersions = GetCompatibleElasticsearchVersions'
  { domainName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | Undocumented member.
getCompatibleElasticsearchVersions_domainName :: Lens.Lens' GetCompatibleElasticsearchVersions (Prelude.Maybe Prelude.Text)
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
            Prelude.<$> ( x Core..?> "CompatibleElasticsearchVersions"
                            Core..!@ Prelude.mempty
                        )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetCompatibleElasticsearchVersions

instance
  Prelude.NFData
    GetCompatibleElasticsearchVersions

instance
  Core.ToHeaders
    GetCompatibleElasticsearchVersions
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    GetCompatibleElasticsearchVersions
  where
  toPath =
    Prelude.const "/2015-01-01/es/compatibleVersions"

instance
  Core.ToQuery
    GetCompatibleElasticsearchVersions
  where
  toQuery GetCompatibleElasticsearchVersions' {..} =
    Prelude.mconcat ["domainName" Core.=: domainName]

-- | Container for response returned by
-- @ GetCompatibleElasticsearchVersions @ operation.
--
-- /See:/ 'newGetCompatibleElasticsearchVersionsResponse' smart constructor.
data GetCompatibleElasticsearchVersionsResponse = GetCompatibleElasticsearchVersionsResponse'
  { -- | A map of compatible Elasticsearch versions returned as part of the
    -- @ GetCompatibleElasticsearchVersions @ operation.
    compatibleElasticsearchVersions :: Prelude.Maybe [CompatibleVersionsMap],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetCompatibleElasticsearchVersionsResponse
newGetCompatibleElasticsearchVersionsResponse
  pHttpStatus_ =
    GetCompatibleElasticsearchVersionsResponse'
      { compatibleElasticsearchVersions =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A map of compatible Elasticsearch versions returned as part of the
-- @ GetCompatibleElasticsearchVersions @ operation.
getCompatibleElasticsearchVersionsResponse_compatibleElasticsearchVersions :: Lens.Lens' GetCompatibleElasticsearchVersionsResponse (Prelude.Maybe [CompatibleVersionsMap])
getCompatibleElasticsearchVersionsResponse_compatibleElasticsearchVersions = Lens.lens (\GetCompatibleElasticsearchVersionsResponse' {compatibleElasticsearchVersions} -> compatibleElasticsearchVersions) (\s@GetCompatibleElasticsearchVersionsResponse' {} a -> s {compatibleElasticsearchVersions = a} :: GetCompatibleElasticsearchVersionsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getCompatibleElasticsearchVersionsResponse_httpStatus :: Lens.Lens' GetCompatibleElasticsearchVersionsResponse Prelude.Int
getCompatibleElasticsearchVersionsResponse_httpStatus = Lens.lens (\GetCompatibleElasticsearchVersionsResponse' {httpStatus} -> httpStatus) (\s@GetCompatibleElasticsearchVersionsResponse' {} a -> s {httpStatus = a} :: GetCompatibleElasticsearchVersionsResponse)

instance
  Prelude.NFData
    GetCompatibleElasticsearchVersionsResponse
