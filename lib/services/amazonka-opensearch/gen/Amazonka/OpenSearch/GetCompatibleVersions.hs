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
-- Module      : Amazonka.OpenSearch.GetCompatibleVersions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a map of OpenSearch or Elasticsearch versions and the versions
-- you can upgrade them to.
module Amazonka.OpenSearch.GetCompatibleVersions
  ( -- * Creating a Request
    GetCompatibleVersions (..),
    newGetCompatibleVersions,

    -- * Request Lenses
    getCompatibleVersions_domainName,

    -- * Destructuring the Response
    GetCompatibleVersionsResponse (..),
    newGetCompatibleVersionsResponse,

    -- * Response Lenses
    getCompatibleVersionsResponse_compatibleVersions,
    getCompatibleVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the request parameters to @GetCompatibleVersions@
-- operation.
--
-- /See:/ 'newGetCompatibleVersions' smart constructor.
data GetCompatibleVersions = GetCompatibleVersions'
  { -- | The name of an existing domain. Provide this parameter to limit the
    -- results to a single domain.
    domainName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCompatibleVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'getCompatibleVersions_domainName' - The name of an existing domain. Provide this parameter to limit the
-- results to a single domain.
newGetCompatibleVersions ::
  GetCompatibleVersions
newGetCompatibleVersions =
  GetCompatibleVersions'
    { domainName =
        Prelude.Nothing
    }

-- | The name of an existing domain. Provide this parameter to limit the
-- results to a single domain.
getCompatibleVersions_domainName :: Lens.Lens' GetCompatibleVersions (Prelude.Maybe Prelude.Text)
getCompatibleVersions_domainName = Lens.lens (\GetCompatibleVersions' {domainName} -> domainName) (\s@GetCompatibleVersions' {} a -> s {domainName = a} :: GetCompatibleVersions)

instance Core.AWSRequest GetCompatibleVersions where
  type
    AWSResponse GetCompatibleVersions =
      GetCompatibleVersionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCompatibleVersionsResponse'
            Prelude.<$> ( x Core..?> "CompatibleVersions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCompatibleVersions where
  hashWithSalt _salt GetCompatibleVersions' {..} =
    _salt `Prelude.hashWithSalt` domainName

instance Prelude.NFData GetCompatibleVersions where
  rnf GetCompatibleVersions' {..} =
    Prelude.rnf domainName

instance Core.ToHeaders GetCompatibleVersions where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetCompatibleVersions where
  toPath =
    Prelude.const
      "/2021-01-01/opensearch/compatibleVersions"

instance Core.ToQuery GetCompatibleVersions where
  toQuery GetCompatibleVersions' {..} =
    Prelude.mconcat ["domainName" Core.=: domainName]

-- | Container for the response returned by the @GetCompatibleVersions@
-- operation.
--
-- /See:/ 'newGetCompatibleVersionsResponse' smart constructor.
data GetCompatibleVersionsResponse = GetCompatibleVersionsResponse'
  { -- | A map of OpenSearch or Elasticsearch versions and the versions you can
    -- upgrade them to.
    compatibleVersions :: Prelude.Maybe [CompatibleVersionsMap],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCompatibleVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compatibleVersions', 'getCompatibleVersionsResponse_compatibleVersions' - A map of OpenSearch or Elasticsearch versions and the versions you can
-- upgrade them to.
--
-- 'httpStatus', 'getCompatibleVersionsResponse_httpStatus' - The response's http status code.
newGetCompatibleVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCompatibleVersionsResponse
newGetCompatibleVersionsResponse pHttpStatus_ =
  GetCompatibleVersionsResponse'
    { compatibleVersions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A map of OpenSearch or Elasticsearch versions and the versions you can
-- upgrade them to.
getCompatibleVersionsResponse_compatibleVersions :: Lens.Lens' GetCompatibleVersionsResponse (Prelude.Maybe [CompatibleVersionsMap])
getCompatibleVersionsResponse_compatibleVersions = Lens.lens (\GetCompatibleVersionsResponse' {compatibleVersions} -> compatibleVersions) (\s@GetCompatibleVersionsResponse' {} a -> s {compatibleVersions = a} :: GetCompatibleVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getCompatibleVersionsResponse_httpStatus :: Lens.Lens' GetCompatibleVersionsResponse Prelude.Int
getCompatibleVersionsResponse_httpStatus = Lens.lens (\GetCompatibleVersionsResponse' {httpStatus} -> httpStatus) (\s@GetCompatibleVersionsResponse' {} a -> s {httpStatus = a} :: GetCompatibleVersionsResponse)

instance Prelude.NFData GetCompatibleVersionsResponse where
  rnf GetCompatibleVersionsResponse' {..} =
    Prelude.rnf compatibleVersions
      `Prelude.seq` Prelude.rnf httpStatus
