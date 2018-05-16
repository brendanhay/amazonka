{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.DeleteElasticsearchServiceRole
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the service-linked role that Elasticsearch Service uses to manage and maintain VPC domains. Role deletion will fail if any existing VPC domains use the role. You must delete any such Elasticsearch domains before deleting the role. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html#es-enabling-slr Deleting Elasticsearch Service Role> in /VPC Endpoints for Amazon Elasticsearch Service Domains/ .
--
--
module Network.AWS.ElasticSearch.DeleteElasticsearchServiceRole
    (
    -- * Creating a Request
      deleteElasticsearchServiceRole
    , DeleteElasticsearchServiceRole

    -- * Destructuring the Response
    , deleteElasticsearchServiceRoleResponse
    , DeleteElasticsearchServiceRoleResponse
    ) where

import Network.AWS.ElasticSearch.Types
import Network.AWS.ElasticSearch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteElasticsearchServiceRole' smart constructor.
data DeleteElasticsearchServiceRole =
  DeleteElasticsearchServiceRole'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteElasticsearchServiceRole' with the minimum fields required to make a request.
--
deleteElasticsearchServiceRole
    :: DeleteElasticsearchServiceRole
deleteElasticsearchServiceRole = DeleteElasticsearchServiceRole'


instance AWSRequest DeleteElasticsearchServiceRole
         where
        type Rs DeleteElasticsearchServiceRole =
             DeleteElasticsearchServiceRoleResponse
        request = delete elasticSearch
        response
          = receiveNull DeleteElasticsearchServiceRoleResponse'

instance Hashable DeleteElasticsearchServiceRole
         where

instance NFData DeleteElasticsearchServiceRole where

instance ToHeaders DeleteElasticsearchServiceRole
         where
        toHeaders = const mempty

instance ToPath DeleteElasticsearchServiceRole where
        toPath = const "/2015-01-01/es/role"

instance ToQuery DeleteElasticsearchServiceRole where
        toQuery = const mempty

-- | /See:/ 'deleteElasticsearchServiceRoleResponse' smart constructor.
data DeleteElasticsearchServiceRoleResponse =
  DeleteElasticsearchServiceRoleResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteElasticsearchServiceRoleResponse' with the minimum fields required to make a request.
--
deleteElasticsearchServiceRoleResponse
    :: DeleteElasticsearchServiceRoleResponse
deleteElasticsearchServiceRoleResponse = DeleteElasticsearchServiceRoleResponse'


instance NFData
           DeleteElasticsearchServiceRoleResponse
         where
