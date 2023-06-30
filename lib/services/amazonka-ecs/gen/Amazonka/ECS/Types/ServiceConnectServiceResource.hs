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
-- Module      : Amazonka.ECS.Types.ServiceConnectServiceResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.ServiceConnectServiceResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Service Connect resource. Each configuration maps a discovery name
-- to a Cloud Map service name. The data is stored in Cloud Map as part of
-- the Service Connect configuration for each discovery name of this Amazon
-- ECS service.
--
-- A task can resolve the @dnsName@ for each of the @clientAliases@ of a
-- service. However a task can\'t resolve the discovery names. If you want
-- to connect to a service, refer to the @ServiceConnectConfiguration@ of
-- that service for the list of @clientAliases@ that you can use.
--
-- /See:/ 'newServiceConnectServiceResource' smart constructor.
data ServiceConnectServiceResource = ServiceConnectServiceResource'
  { -- | The Amazon Resource Name (ARN) for the namespace in Cloud Map that
    -- matches the discovery name for this Service Connect resource. You can
    -- use this ARN in other integrations with Cloud Map. However, Service
    -- Connect can\'t ensure connectivity outside of Amazon ECS.
    discoveryArn :: Prelude.Maybe Prelude.Text,
    -- | The discovery name of this Service Connect resource.
    --
    -- The @discoveryName@ is the name of the new Cloud Map service that Amazon
    -- ECS creates for this Amazon ECS service. This must be unique within the
    -- Cloud Map namespace. The name can contain up to 64 characters. The name
    -- can include lowercase letters, numbers, underscores (_), and hyphens
    -- (-). The name can\'t start with a hyphen.
    --
    -- If this parameter isn\'t specified, the default value of
    -- @discoveryName.namespace@ is used. If the @discoveryName@ isn\'t
    -- specified, the port mapping name from the task definition is used in
    -- @portName.namespace@.
    discoveryName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceConnectServiceResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'discoveryArn', 'serviceConnectServiceResource_discoveryArn' - The Amazon Resource Name (ARN) for the namespace in Cloud Map that
-- matches the discovery name for this Service Connect resource. You can
-- use this ARN in other integrations with Cloud Map. However, Service
-- Connect can\'t ensure connectivity outside of Amazon ECS.
--
-- 'discoveryName', 'serviceConnectServiceResource_discoveryName' - The discovery name of this Service Connect resource.
--
-- The @discoveryName@ is the name of the new Cloud Map service that Amazon
-- ECS creates for this Amazon ECS service. This must be unique within the
-- Cloud Map namespace. The name can contain up to 64 characters. The name
-- can include lowercase letters, numbers, underscores (_), and hyphens
-- (-). The name can\'t start with a hyphen.
--
-- If this parameter isn\'t specified, the default value of
-- @discoveryName.namespace@ is used. If the @discoveryName@ isn\'t
-- specified, the port mapping name from the task definition is used in
-- @portName.namespace@.
newServiceConnectServiceResource ::
  ServiceConnectServiceResource
newServiceConnectServiceResource =
  ServiceConnectServiceResource'
    { discoveryArn =
        Prelude.Nothing,
      discoveryName = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) for the namespace in Cloud Map that
-- matches the discovery name for this Service Connect resource. You can
-- use this ARN in other integrations with Cloud Map. However, Service
-- Connect can\'t ensure connectivity outside of Amazon ECS.
serviceConnectServiceResource_discoveryArn :: Lens.Lens' ServiceConnectServiceResource (Prelude.Maybe Prelude.Text)
serviceConnectServiceResource_discoveryArn = Lens.lens (\ServiceConnectServiceResource' {discoveryArn} -> discoveryArn) (\s@ServiceConnectServiceResource' {} a -> s {discoveryArn = a} :: ServiceConnectServiceResource)

-- | The discovery name of this Service Connect resource.
--
-- The @discoveryName@ is the name of the new Cloud Map service that Amazon
-- ECS creates for this Amazon ECS service. This must be unique within the
-- Cloud Map namespace. The name can contain up to 64 characters. The name
-- can include lowercase letters, numbers, underscores (_), and hyphens
-- (-). The name can\'t start with a hyphen.
--
-- If this parameter isn\'t specified, the default value of
-- @discoveryName.namespace@ is used. If the @discoveryName@ isn\'t
-- specified, the port mapping name from the task definition is used in
-- @portName.namespace@.
serviceConnectServiceResource_discoveryName :: Lens.Lens' ServiceConnectServiceResource (Prelude.Maybe Prelude.Text)
serviceConnectServiceResource_discoveryName = Lens.lens (\ServiceConnectServiceResource' {discoveryName} -> discoveryName) (\s@ServiceConnectServiceResource' {} a -> s {discoveryName = a} :: ServiceConnectServiceResource)

instance Data.FromJSON ServiceConnectServiceResource where
  parseJSON =
    Data.withObject
      "ServiceConnectServiceResource"
      ( \x ->
          ServiceConnectServiceResource'
            Prelude.<$> (x Data..:? "discoveryArn")
            Prelude.<*> (x Data..:? "discoveryName")
      )

instance
  Prelude.Hashable
    ServiceConnectServiceResource
  where
  hashWithSalt _salt ServiceConnectServiceResource' {..} =
    _salt
      `Prelude.hashWithSalt` discoveryArn
      `Prelude.hashWithSalt` discoveryName

instance Prelude.NFData ServiceConnectServiceResource where
  rnf ServiceConnectServiceResource' {..} =
    Prelude.rnf discoveryArn
      `Prelude.seq` Prelude.rnf discoveryName
