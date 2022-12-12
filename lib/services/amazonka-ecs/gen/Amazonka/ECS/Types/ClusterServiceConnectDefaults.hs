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
-- Module      : Amazonka.ECS.Types.ClusterServiceConnectDefaults
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.ClusterServiceConnectDefaults where

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
-- /See:/ 'newClusterServiceConnectDefaults' smart constructor.
data ClusterServiceConnectDefaults = ClusterServiceConnectDefaults'
  { -- | The namespace name or full Amazon Resource Name (ARN) of the Cloud Map
    -- namespace. When you create a service and don\'t specify a Service
    -- Connect configuration, this namespace is used.
    namespace :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClusterServiceConnectDefaults' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namespace', 'clusterServiceConnectDefaults_namespace' - The namespace name or full Amazon Resource Name (ARN) of the Cloud Map
-- namespace. When you create a service and don\'t specify a Service
-- Connect configuration, this namespace is used.
newClusterServiceConnectDefaults ::
  ClusterServiceConnectDefaults
newClusterServiceConnectDefaults =
  ClusterServiceConnectDefaults'
    { namespace =
        Prelude.Nothing
    }

-- | The namespace name or full Amazon Resource Name (ARN) of the Cloud Map
-- namespace. When you create a service and don\'t specify a Service
-- Connect configuration, this namespace is used.
clusterServiceConnectDefaults_namespace :: Lens.Lens' ClusterServiceConnectDefaults (Prelude.Maybe Prelude.Text)
clusterServiceConnectDefaults_namespace = Lens.lens (\ClusterServiceConnectDefaults' {namespace} -> namespace) (\s@ClusterServiceConnectDefaults' {} a -> s {namespace = a} :: ClusterServiceConnectDefaults)

instance Data.FromJSON ClusterServiceConnectDefaults where
  parseJSON =
    Data.withObject
      "ClusterServiceConnectDefaults"
      ( \x ->
          ClusterServiceConnectDefaults'
            Prelude.<$> (x Data..:? "namespace")
      )

instance
  Prelude.Hashable
    ClusterServiceConnectDefaults
  where
  hashWithSalt _salt ClusterServiceConnectDefaults' {..} =
    _salt `Prelude.hashWithSalt` namespace

instance Prelude.NFData ClusterServiceConnectDefaults where
  rnf ClusterServiceConnectDefaults' {..} =
    Prelude.rnf namespace
