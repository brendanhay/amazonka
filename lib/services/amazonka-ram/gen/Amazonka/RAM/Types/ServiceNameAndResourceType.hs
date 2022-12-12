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
-- Module      : Amazonka.RAM.Types.ServiceNameAndResourceType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RAM.Types.ServiceNameAndResourceType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types.ResourceRegionScope

-- | Information about a shareable resource type and the Amazon Web Services
-- service to which resources of that type belong.
--
-- /See:/ 'newServiceNameAndResourceType' smart constructor.
data ServiceNameAndResourceType = ServiceNameAndResourceType'
  { -- | Specifies the scope of visibility of resources of this type:
    --
    -- -   __REGIONAL__ – The resource can be accessed only by using requests
    --     that target the Amazon Web Services Region in which the resource
    --     exists.
    --
    -- -   __GLOBAL__ – The resource can be accessed from any Amazon Web
    --     Services Region.
    resourceRegionScope :: Prelude.Maybe ResourceRegionScope,
    -- | The type of the resource.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon Web Services service to which resources of this
    -- type belong.
    serviceName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceNameAndResourceType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceRegionScope', 'serviceNameAndResourceType_resourceRegionScope' - Specifies the scope of visibility of resources of this type:
--
-- -   __REGIONAL__ – The resource can be accessed only by using requests
--     that target the Amazon Web Services Region in which the resource
--     exists.
--
-- -   __GLOBAL__ – The resource can be accessed from any Amazon Web
--     Services Region.
--
-- 'resourceType', 'serviceNameAndResourceType_resourceType' - The type of the resource.
--
-- 'serviceName', 'serviceNameAndResourceType_serviceName' - The name of the Amazon Web Services service to which resources of this
-- type belong.
newServiceNameAndResourceType ::
  ServiceNameAndResourceType
newServiceNameAndResourceType =
  ServiceNameAndResourceType'
    { resourceRegionScope =
        Prelude.Nothing,
      resourceType = Prelude.Nothing,
      serviceName = Prelude.Nothing
    }

-- | Specifies the scope of visibility of resources of this type:
--
-- -   __REGIONAL__ – The resource can be accessed only by using requests
--     that target the Amazon Web Services Region in which the resource
--     exists.
--
-- -   __GLOBAL__ – The resource can be accessed from any Amazon Web
--     Services Region.
serviceNameAndResourceType_resourceRegionScope :: Lens.Lens' ServiceNameAndResourceType (Prelude.Maybe ResourceRegionScope)
serviceNameAndResourceType_resourceRegionScope = Lens.lens (\ServiceNameAndResourceType' {resourceRegionScope} -> resourceRegionScope) (\s@ServiceNameAndResourceType' {} a -> s {resourceRegionScope = a} :: ServiceNameAndResourceType)

-- | The type of the resource.
serviceNameAndResourceType_resourceType :: Lens.Lens' ServiceNameAndResourceType (Prelude.Maybe Prelude.Text)
serviceNameAndResourceType_resourceType = Lens.lens (\ServiceNameAndResourceType' {resourceType} -> resourceType) (\s@ServiceNameAndResourceType' {} a -> s {resourceType = a} :: ServiceNameAndResourceType)

-- | The name of the Amazon Web Services service to which resources of this
-- type belong.
serviceNameAndResourceType_serviceName :: Lens.Lens' ServiceNameAndResourceType (Prelude.Maybe Prelude.Text)
serviceNameAndResourceType_serviceName = Lens.lens (\ServiceNameAndResourceType' {serviceName} -> serviceName) (\s@ServiceNameAndResourceType' {} a -> s {serviceName = a} :: ServiceNameAndResourceType)

instance Data.FromJSON ServiceNameAndResourceType where
  parseJSON =
    Data.withObject
      "ServiceNameAndResourceType"
      ( \x ->
          ServiceNameAndResourceType'
            Prelude.<$> (x Data..:? "resourceRegionScope")
            Prelude.<*> (x Data..:? "resourceType")
            Prelude.<*> (x Data..:? "serviceName")
      )

instance Prelude.Hashable ServiceNameAndResourceType where
  hashWithSalt _salt ServiceNameAndResourceType' {..} =
    _salt `Prelude.hashWithSalt` resourceRegionScope
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` serviceName

instance Prelude.NFData ServiceNameAndResourceType where
  rnf ServiceNameAndResourceType' {..} =
    Prelude.rnf resourceRegionScope
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf serviceName
