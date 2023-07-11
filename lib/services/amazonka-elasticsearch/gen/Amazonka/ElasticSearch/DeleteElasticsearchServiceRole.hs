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
-- Module      : Amazonka.ElasticSearch.DeleteElasticsearchServiceRole
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the service-linked role that Elasticsearch Service uses to
-- manage and maintain VPC domains. Role deletion will fail if any existing
-- VPC domains use the role. You must delete any such Elasticsearch domains
-- before deleting the role. See
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html#es-enabling-slr Deleting Elasticsearch Service Role>
-- in /VPC Endpoints for Amazon Elasticsearch Service Domains/.
module Amazonka.ElasticSearch.DeleteElasticsearchServiceRole
  ( -- * Creating a Request
    DeleteElasticsearchServiceRole (..),
    newDeleteElasticsearchServiceRole,

    -- * Destructuring the Response
    DeleteElasticsearchServiceRoleResponse (..),
    newDeleteElasticsearchServiceRoleResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteElasticsearchServiceRole' smart constructor.
data DeleteElasticsearchServiceRole = DeleteElasticsearchServiceRole'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteElasticsearchServiceRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteElasticsearchServiceRole ::
  DeleteElasticsearchServiceRole
newDeleteElasticsearchServiceRole =
  DeleteElasticsearchServiceRole'

instance
  Core.AWSRequest
    DeleteElasticsearchServiceRole
  where
  type
    AWSResponse DeleteElasticsearchServiceRole =
      DeleteElasticsearchServiceRoleResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteElasticsearchServiceRoleResponse'

instance
  Prelude.Hashable
    DeleteElasticsearchServiceRole
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    DeleteElasticsearchServiceRole
  where
  rnf _ = ()

instance
  Data.ToHeaders
    DeleteElasticsearchServiceRole
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteElasticsearchServiceRole where
  toPath = Prelude.const "/2015-01-01/es/role"

instance Data.ToQuery DeleteElasticsearchServiceRole where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteElasticsearchServiceRoleResponse' smart constructor.
data DeleteElasticsearchServiceRoleResponse = DeleteElasticsearchServiceRoleResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteElasticsearchServiceRoleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteElasticsearchServiceRoleResponse ::
  DeleteElasticsearchServiceRoleResponse
newDeleteElasticsearchServiceRoleResponse =
  DeleteElasticsearchServiceRoleResponse'

instance
  Prelude.NFData
    DeleteElasticsearchServiceRoleResponse
  where
  rnf _ = ()
