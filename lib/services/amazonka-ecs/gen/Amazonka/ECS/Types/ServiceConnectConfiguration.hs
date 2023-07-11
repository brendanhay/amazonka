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
-- Module      : Amazonka.ECS.Types.ServiceConnectConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.ServiceConnectConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types.LogConfiguration
import Amazonka.ECS.Types.ServiceConnectService
import qualified Amazonka.Prelude as Prelude

-- | The Service Connect configuration of your Amazon ECS service. The
-- configuration for this service to discover and connect to services, and
-- be discovered by, and connected from, other services within a namespace.
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
-- /See:/ 'newServiceConnectConfiguration' smart constructor.
data ServiceConnectConfiguration = ServiceConnectConfiguration'
  { logConfiguration :: Prelude.Maybe LogConfiguration,
    -- | The namespace name or full Amazon Resource Name (ARN) of the Cloud Map
    -- namespace for use with Service Connect. The namespace must be in the
    -- same Amazon Web Services Region as the Amazon ECS service and cluster.
    -- The type of namespace doesn\'t affect Service Connect. For more
    -- information about Cloud Map, see
    -- <https://docs.aws.amazon.com/ Working with Services> in the /Cloud Map
    -- Developer Guide/.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The list of Service Connect service objects. These are names and aliases
    -- (also known as endpoints) that are used by other Amazon ECS services to
    -- connect to this service.
    --
    -- This field is not required for a \"client\" Amazon ECS service that\'s a
    -- member of a namespace only to connect to other services within the
    -- namespace. An example of this would be a frontend application that
    -- accepts incoming requests from either a load balancer that\'s attached
    -- to the service or by other means.
    --
    -- An object selects a port from the task definition, assigns a name for
    -- the Cloud Map service, and a list of aliases (endpoints) and ports for
    -- client applications to refer to this service.
    services :: Prelude.Maybe [ServiceConnectService],
    -- | Specifies whether to use Service Connect with this service.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceConnectConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logConfiguration', 'serviceConnectConfiguration_logConfiguration' - Undocumented member.
--
-- 'namespace', 'serviceConnectConfiguration_namespace' - The namespace name or full Amazon Resource Name (ARN) of the Cloud Map
-- namespace for use with Service Connect. The namespace must be in the
-- same Amazon Web Services Region as the Amazon ECS service and cluster.
-- The type of namespace doesn\'t affect Service Connect. For more
-- information about Cloud Map, see
-- <https://docs.aws.amazon.com/ Working with Services> in the /Cloud Map
-- Developer Guide/.
--
-- 'services', 'serviceConnectConfiguration_services' - The list of Service Connect service objects. These are names and aliases
-- (also known as endpoints) that are used by other Amazon ECS services to
-- connect to this service.
--
-- This field is not required for a \"client\" Amazon ECS service that\'s a
-- member of a namespace only to connect to other services within the
-- namespace. An example of this would be a frontend application that
-- accepts incoming requests from either a load balancer that\'s attached
-- to the service or by other means.
--
-- An object selects a port from the task definition, assigns a name for
-- the Cloud Map service, and a list of aliases (endpoints) and ports for
-- client applications to refer to this service.
--
-- 'enabled', 'serviceConnectConfiguration_enabled' - Specifies whether to use Service Connect with this service.
newServiceConnectConfiguration ::
  -- | 'enabled'
  Prelude.Bool ->
  ServiceConnectConfiguration
newServiceConnectConfiguration pEnabled_ =
  ServiceConnectConfiguration'
    { logConfiguration =
        Prelude.Nothing,
      namespace = Prelude.Nothing,
      services = Prelude.Nothing,
      enabled = pEnabled_
    }

-- | Undocumented member.
serviceConnectConfiguration_logConfiguration :: Lens.Lens' ServiceConnectConfiguration (Prelude.Maybe LogConfiguration)
serviceConnectConfiguration_logConfiguration = Lens.lens (\ServiceConnectConfiguration' {logConfiguration} -> logConfiguration) (\s@ServiceConnectConfiguration' {} a -> s {logConfiguration = a} :: ServiceConnectConfiguration)

-- | The namespace name or full Amazon Resource Name (ARN) of the Cloud Map
-- namespace for use with Service Connect. The namespace must be in the
-- same Amazon Web Services Region as the Amazon ECS service and cluster.
-- The type of namespace doesn\'t affect Service Connect. For more
-- information about Cloud Map, see
-- <https://docs.aws.amazon.com/ Working with Services> in the /Cloud Map
-- Developer Guide/.
serviceConnectConfiguration_namespace :: Lens.Lens' ServiceConnectConfiguration (Prelude.Maybe Prelude.Text)
serviceConnectConfiguration_namespace = Lens.lens (\ServiceConnectConfiguration' {namespace} -> namespace) (\s@ServiceConnectConfiguration' {} a -> s {namespace = a} :: ServiceConnectConfiguration)

-- | The list of Service Connect service objects. These are names and aliases
-- (also known as endpoints) that are used by other Amazon ECS services to
-- connect to this service.
--
-- This field is not required for a \"client\" Amazon ECS service that\'s a
-- member of a namespace only to connect to other services within the
-- namespace. An example of this would be a frontend application that
-- accepts incoming requests from either a load balancer that\'s attached
-- to the service or by other means.
--
-- An object selects a port from the task definition, assigns a name for
-- the Cloud Map service, and a list of aliases (endpoints) and ports for
-- client applications to refer to this service.
serviceConnectConfiguration_services :: Lens.Lens' ServiceConnectConfiguration (Prelude.Maybe [ServiceConnectService])
serviceConnectConfiguration_services = Lens.lens (\ServiceConnectConfiguration' {services} -> services) (\s@ServiceConnectConfiguration' {} a -> s {services = a} :: ServiceConnectConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether to use Service Connect with this service.
serviceConnectConfiguration_enabled :: Lens.Lens' ServiceConnectConfiguration Prelude.Bool
serviceConnectConfiguration_enabled = Lens.lens (\ServiceConnectConfiguration' {enabled} -> enabled) (\s@ServiceConnectConfiguration' {} a -> s {enabled = a} :: ServiceConnectConfiguration)

instance Data.FromJSON ServiceConnectConfiguration where
  parseJSON =
    Data.withObject
      "ServiceConnectConfiguration"
      ( \x ->
          ServiceConnectConfiguration'
            Prelude.<$> (x Data..:? "logConfiguration")
            Prelude.<*> (x Data..:? "namespace")
            Prelude.<*> (x Data..:? "services" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "enabled")
      )

instance Prelude.Hashable ServiceConnectConfiguration where
  hashWithSalt _salt ServiceConnectConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` logConfiguration
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` services
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData ServiceConnectConfiguration where
  rnf ServiceConnectConfiguration' {..} =
    Prelude.rnf logConfiguration
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf services
      `Prelude.seq` Prelude.rnf enabled

instance Data.ToJSON ServiceConnectConfiguration where
  toJSON ServiceConnectConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("logConfiguration" Data..=)
              Prelude.<$> logConfiguration,
            ("namespace" Data..=) Prelude.<$> namespace,
            ("services" Data..=) Prelude.<$> services,
            Prelude.Just ("enabled" Data..= enabled)
          ]
      )
