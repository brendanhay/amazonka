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

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteElasticsearchServiceRole' smart constructor.
data DeleteElasticsearchServiceRole = DeleteElasticsearchServiceRole'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteElasticsearchServiceRole' value with any optional fields omitted.
mkDeleteElasticsearchServiceRole ::
  DeleteElasticsearchServiceRole
mkDeleteElasticsearchServiceRole = DeleteElasticsearchServiceRole'

instance Core.AWSRequest DeleteElasticsearchServiceRole where
  type
    Rs DeleteElasticsearchServiceRole =
      DeleteElasticsearchServiceRoleResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath = Core.rawPath "/2015-01-01/es/role",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveNull DeleteElasticsearchServiceRoleResponse'

-- | /See:/ 'mkDeleteElasticsearchServiceRoleResponse' smart constructor.
data DeleteElasticsearchServiceRoleResponse = DeleteElasticsearchServiceRoleResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteElasticsearchServiceRoleResponse' value with any optional fields omitted.
mkDeleteElasticsearchServiceRoleResponse ::
  DeleteElasticsearchServiceRoleResponse
mkDeleteElasticsearchServiceRoleResponse =
  DeleteElasticsearchServiceRoleResponse'
