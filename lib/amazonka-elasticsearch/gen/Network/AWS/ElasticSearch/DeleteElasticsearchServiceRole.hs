{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.DeleteElasticsearchServiceRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the service-linked role that Elasticsearch Service uses to manage and maintain VPC domains. Role deletion will fail if any existing VPC domains use the role. You must delete any such Elasticsearch domains before deleting the role. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html#es-enabling-slr Deleting Elasticsearch Service Role> in /VPC Endpoints for Amazon Elasticsearch Service Domains/ .
module Network.AWS.ElasticSearch.DeleteElasticsearchServiceRole
  ( -- * Creating a request
    DeleteElasticsearchServiceRole (..),
    mkDeleteElasticsearchServiceRole,

    -- * Destructuring the response
    DeleteElasticsearchServiceRoleResponse (..),
    mkDeleteElasticsearchServiceRoleResponse,
  )
where

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteElasticsearchServiceRole' smart constructor.
data DeleteElasticsearchServiceRole = DeleteElasticsearchServiceRole'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteElasticsearchServiceRole' with the minimum fields required to make a request.
mkDeleteElasticsearchServiceRole ::
  DeleteElasticsearchServiceRole
mkDeleteElasticsearchServiceRole = DeleteElasticsearchServiceRole'

instance Lude.AWSRequest DeleteElasticsearchServiceRole where
  type
    Rs DeleteElasticsearchServiceRole =
      DeleteElasticsearchServiceRoleResponse
  request = Req.delete elasticSearchService
  response = Res.receiveNull DeleteElasticsearchServiceRoleResponse'

instance Lude.ToHeaders DeleteElasticsearchServiceRole where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteElasticsearchServiceRole where
  toPath = Lude.const "/2015-01-01/es/role"

instance Lude.ToQuery DeleteElasticsearchServiceRole where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteElasticsearchServiceRoleResponse' smart constructor.
data DeleteElasticsearchServiceRoleResponse = DeleteElasticsearchServiceRoleResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteElasticsearchServiceRoleResponse' with the minimum fields required to make a request.
mkDeleteElasticsearchServiceRoleResponse ::
  DeleteElasticsearchServiceRoleResponse
mkDeleteElasticsearchServiceRoleResponse =
  DeleteElasticsearchServiceRoleResponse'
