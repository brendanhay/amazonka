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
-- Module      : Network.AWS.ElasticSearch.DeleteElasticsearchServiceRole
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the service-linked role that Elasticsearch Service uses to
-- manage and maintain VPC domains. Role deletion will fail if any existing
-- VPC domains use the role. You must delete any such Elasticsearch domains
-- before deleting the role. See
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html#es-enabling-slr Deleting Elasticsearch Service Role>
-- in /VPC Endpoints for Amazon Elasticsearch Service Domains/.
module Network.AWS.ElasticSearch.DeleteElasticsearchServiceRole
  ( -- * Creating a Request
    DeleteElasticsearchServiceRole (..),
    newDeleteElasticsearchServiceRole,

    -- * Destructuring the Response
    DeleteElasticsearchServiceRoleResponse (..),
    newDeleteElasticsearchServiceRoleResponse,
  )
where

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteElasticsearchServiceRole' smart constructor.
data DeleteElasticsearchServiceRole = DeleteElasticsearchServiceRole'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteElasticsearchServiceRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteElasticsearchServiceRole ::
  DeleteElasticsearchServiceRole
newDeleteElasticsearchServiceRole =
  DeleteElasticsearchServiceRole'

instance
  Prelude.AWSRequest
    DeleteElasticsearchServiceRole
  where
  type
    Rs DeleteElasticsearchServiceRole =
      DeleteElasticsearchServiceRoleResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull
      DeleteElasticsearchServiceRoleResponse'

instance
  Prelude.Hashable
    DeleteElasticsearchServiceRole

instance
  Prelude.NFData
    DeleteElasticsearchServiceRole

instance
  Prelude.ToHeaders
    DeleteElasticsearchServiceRole
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    DeleteElasticsearchServiceRole
  where
  toPath = Prelude.const "/2015-01-01/es/role"

instance
  Prelude.ToQuery
    DeleteElasticsearchServiceRole
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteElasticsearchServiceRoleResponse' smart constructor.
data DeleteElasticsearchServiceRoleResponse = DeleteElasticsearchServiceRoleResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
