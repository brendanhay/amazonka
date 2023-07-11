{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ECS.Types.ClusterServiceConnectDefaultsRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.ClusterServiceConnectDefaultsRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Use this parameter to set a default Service Connect namespace. After you
-- set a default Service Connect namespace, any new services with Service
-- Connect turned on that are created in the cluster are added as client
-- services in the namespace. This setting only applies to new services
-- that set the @enabled@ parameter to @true@ in the
-- @ServiceConnectConfiguration@. You can set the namespace of each service
-- individually in the @ServiceConnectConfiguration@ to override this
-- default parameter.
--
-- Tasks that run in a namespace can use short names to connect to services
-- in the namespace. Tasks can connect to services across all of the
-- clusters in the namespace. Tasks connect through a managed proxy
-- container that collects logs and metrics for increased visibility. Only
-- the tasks that Amazon ECS services create are supported with Service
-- Connect. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-connect.html Service Connect>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- /See:/ 'newClusterServiceConnectDefaultsRequest' smart constructor.
data ClusterServiceConnectDefaultsRequest = ClusterServiceConnectDefaultsRequest'
  { -- | The namespace name or full Amazon Resource Name (ARN) of the Cloud Map
    -- namespace that\'s used when you create a service and don\'t specify a
    -- Service Connect configuration. The namespace name can include up to 1024
    -- characters. The name is case-sensitive. The name can\'t include hyphens
    -- (-), tilde (~), greater than (>), less than (\<), or slash (\/).
    --
    -- If you enter an existing namespace name or ARN, then that namespace will
    -- be used. Any namespace type is supported. The namespace must be in this
    -- account and this Amazon Web Services Region.
    --
    -- If you enter a new name, a Cloud Map namespace will be created. Amazon
    -- ECS creates a Cloud Map namespace with the \"API calls\" method of
    -- instance discovery only. This instance discovery method is the \"HTTP\"
    -- namespace type in the Command Line Interface. Other types of instance
    -- discovery aren\'t used by Service Connect.
    --
    -- If you update the service with an empty string @\"\"@ for the namespace
    -- name, the cluster configuration for Service Connect is removed. Note
    -- that the namespace will remain in Cloud Map and must be deleted
    -- separately.
    --
    -- For more information about Cloud Map, see
    -- <https://docs.aws.amazon.com/ Working with Services> in the /Cloud Map
    -- Developer Guide/.
    namespace :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClusterServiceConnectDefaultsRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namespace', 'clusterServiceConnectDefaultsRequest_namespace' - The namespace name or full Amazon Resource Name (ARN) of the Cloud Map
-- namespace that\'s used when you create a service and don\'t specify a
-- Service Connect configuration. The namespace name can include up to 1024
-- characters. The name is case-sensitive. The name can\'t include hyphens
-- (-), tilde (~), greater than (>), less than (\<), or slash (\/).
--
-- If you enter an existing namespace name or ARN, then that namespace will
-- be used. Any namespace type is supported. The namespace must be in this
-- account and this Amazon Web Services Region.
--
-- If you enter a new name, a Cloud Map namespace will be created. Amazon
-- ECS creates a Cloud Map namespace with the \"API calls\" method of
-- instance discovery only. This instance discovery method is the \"HTTP\"
-- namespace type in the Command Line Interface. Other types of instance
-- discovery aren\'t used by Service Connect.
--
-- If you update the service with an empty string @\"\"@ for the namespace
-- name, the cluster configuration for Service Connect is removed. Note
-- that the namespace will remain in Cloud Map and must be deleted
-- separately.
--
-- For more information about Cloud Map, see
-- <https://docs.aws.amazon.com/ Working with Services> in the /Cloud Map
-- Developer Guide/.
newClusterServiceConnectDefaultsRequest ::
  -- | 'namespace'
  Prelude.Text ->
  ClusterServiceConnectDefaultsRequest
newClusterServiceConnectDefaultsRequest pNamespace_ =
  ClusterServiceConnectDefaultsRequest'
    { namespace =
        pNamespace_
    }

-- | The namespace name or full Amazon Resource Name (ARN) of the Cloud Map
-- namespace that\'s used when you create a service and don\'t specify a
-- Service Connect configuration. The namespace name can include up to 1024
-- characters. The name is case-sensitive. The name can\'t include hyphens
-- (-), tilde (~), greater than (>), less than (\<), or slash (\/).
--
-- If you enter an existing namespace name or ARN, then that namespace will
-- be used. Any namespace type is supported. The namespace must be in this
-- account and this Amazon Web Services Region.
--
-- If you enter a new name, a Cloud Map namespace will be created. Amazon
-- ECS creates a Cloud Map namespace with the \"API calls\" method of
-- instance discovery only. This instance discovery method is the \"HTTP\"
-- namespace type in the Command Line Interface. Other types of instance
-- discovery aren\'t used by Service Connect.
--
-- If you update the service with an empty string @\"\"@ for the namespace
-- name, the cluster configuration for Service Connect is removed. Note
-- that the namespace will remain in Cloud Map and must be deleted
-- separately.
--
-- For more information about Cloud Map, see
-- <https://docs.aws.amazon.com/ Working with Services> in the /Cloud Map
-- Developer Guide/.
clusterServiceConnectDefaultsRequest_namespace :: Lens.Lens' ClusterServiceConnectDefaultsRequest Prelude.Text
clusterServiceConnectDefaultsRequest_namespace = Lens.lens (\ClusterServiceConnectDefaultsRequest' {namespace} -> namespace) (\s@ClusterServiceConnectDefaultsRequest' {} a -> s {namespace = a} :: ClusterServiceConnectDefaultsRequest)

instance
  Prelude.Hashable
    ClusterServiceConnectDefaultsRequest
  where
  hashWithSalt
    _salt
    ClusterServiceConnectDefaultsRequest' {..} =
      _salt `Prelude.hashWithSalt` namespace

instance
  Prelude.NFData
    ClusterServiceConnectDefaultsRequest
  where
  rnf ClusterServiceConnectDefaultsRequest' {..} =
    Prelude.rnf namespace

instance
  Data.ToJSON
    ClusterServiceConnectDefaultsRequest
  where
  toJSON ClusterServiceConnectDefaultsRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("namespace" Data..= namespace)]
      )
