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
-- Module      : Amazonka.ElasticSearch.DeleteElasticsearchDomain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes the specified Elasticsearch domain and all of its
-- data. Once a domain is deleted, it cannot be recovered.
module Amazonka.ElasticSearch.DeleteElasticsearchDomain
  ( -- * Creating a Request
    DeleteElasticsearchDomain (..),
    newDeleteElasticsearchDomain,

    -- * Request Lenses
    deleteElasticsearchDomain_domainName,

    -- * Destructuring the Response
    DeleteElasticsearchDomainResponse (..),
    newDeleteElasticsearchDomainResponse,

    -- * Response Lenses
    deleteElasticsearchDomainResponse_domainStatus,
    deleteElasticsearchDomainResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @DeleteElasticsearchDomain@
-- operation. Specifies the name of the Elasticsearch domain that you want
-- to delete.
--
-- /See:/ 'newDeleteElasticsearchDomain' smart constructor.
data DeleteElasticsearchDomain = DeleteElasticsearchDomain'
  { -- | The name of the Elasticsearch domain that you want to permanently
    -- delete.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteElasticsearchDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'deleteElasticsearchDomain_domainName' - The name of the Elasticsearch domain that you want to permanently
-- delete.
newDeleteElasticsearchDomain ::
  -- | 'domainName'
  Prelude.Text ->
  DeleteElasticsearchDomain
newDeleteElasticsearchDomain pDomainName_ =
  DeleteElasticsearchDomain'
    { domainName =
        pDomainName_
    }

-- | The name of the Elasticsearch domain that you want to permanently
-- delete.
deleteElasticsearchDomain_domainName :: Lens.Lens' DeleteElasticsearchDomain Prelude.Text
deleteElasticsearchDomain_domainName = Lens.lens (\DeleteElasticsearchDomain' {domainName} -> domainName) (\s@DeleteElasticsearchDomain' {} a -> s {domainName = a} :: DeleteElasticsearchDomain)

instance Core.AWSRequest DeleteElasticsearchDomain where
  type
    AWSResponse DeleteElasticsearchDomain =
      DeleteElasticsearchDomainResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteElasticsearchDomainResponse'
            Prelude.<$> (x Data..?> "DomainStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteElasticsearchDomain where
  hashWithSalt _salt DeleteElasticsearchDomain' {..} =
    _salt `Prelude.hashWithSalt` domainName

instance Prelude.NFData DeleteElasticsearchDomain where
  rnf DeleteElasticsearchDomain' {..} =
    Prelude.rnf domainName

instance Data.ToHeaders DeleteElasticsearchDomain where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteElasticsearchDomain where
  toPath DeleteElasticsearchDomain' {..} =
    Prelude.mconcat
      ["/2015-01-01/es/domain/", Data.toBS domainName]

instance Data.ToQuery DeleteElasticsearchDomain where
  toQuery = Prelude.const Prelude.mempty

-- | The result of a @DeleteElasticsearchDomain@ request. Contains the status
-- of the pending deletion, or no status if the domain and all of its
-- resources have been deleted.
--
-- /See:/ 'newDeleteElasticsearchDomainResponse' smart constructor.
data DeleteElasticsearchDomainResponse = DeleteElasticsearchDomainResponse'
  { -- | The status of the Elasticsearch domain being deleted.
    domainStatus :: Prelude.Maybe ElasticsearchDomainStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteElasticsearchDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainStatus', 'deleteElasticsearchDomainResponse_domainStatus' - The status of the Elasticsearch domain being deleted.
--
-- 'httpStatus', 'deleteElasticsearchDomainResponse_httpStatus' - The response's http status code.
newDeleteElasticsearchDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteElasticsearchDomainResponse
newDeleteElasticsearchDomainResponse pHttpStatus_ =
  DeleteElasticsearchDomainResponse'
    { domainStatus =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the Elasticsearch domain being deleted.
deleteElasticsearchDomainResponse_domainStatus :: Lens.Lens' DeleteElasticsearchDomainResponse (Prelude.Maybe ElasticsearchDomainStatus)
deleteElasticsearchDomainResponse_domainStatus = Lens.lens (\DeleteElasticsearchDomainResponse' {domainStatus} -> domainStatus) (\s@DeleteElasticsearchDomainResponse' {} a -> s {domainStatus = a} :: DeleteElasticsearchDomainResponse)

-- | The response's http status code.
deleteElasticsearchDomainResponse_httpStatus :: Lens.Lens' DeleteElasticsearchDomainResponse Prelude.Int
deleteElasticsearchDomainResponse_httpStatus = Lens.lens (\DeleteElasticsearchDomainResponse' {httpStatus} -> httpStatus) (\s@DeleteElasticsearchDomainResponse' {} a -> s {httpStatus = a} :: DeleteElasticsearchDomainResponse)

instance
  Prelude.NFData
    DeleteElasticsearchDomainResponse
  where
  rnf DeleteElasticsearchDomainResponse' {..} =
    Prelude.rnf domainStatus
      `Prelude.seq` Prelude.rnf httpStatus
