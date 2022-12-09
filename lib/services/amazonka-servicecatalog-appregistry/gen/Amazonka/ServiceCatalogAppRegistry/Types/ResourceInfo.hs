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
-- Module      : Amazonka.ServiceCatalogAppRegistry.Types.ResourceInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalogAppRegistry.Types.ResourceInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceCatalogAppRegistry.Types.ResourceDetails
import Amazonka.ServiceCatalogAppRegistry.Types.ResourceType

-- | The information about the resource.
--
-- /See:/ 'newResourceInfo' smart constructor.
data ResourceInfo = ResourceInfo'
  { -- | The Amazon resource name (ARN) that specifies the resource across
    -- services.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource.
    name :: Prelude.Maybe Prelude.Text,
    -- | The details related to the resource.
    resourceDetails :: Prelude.Maybe ResourceDetails,
    -- | Provides information about the Service Catalog App Registry resource
    -- type.
    resourceType :: Prelude.Maybe ResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'resourceInfo_arn' - The Amazon resource name (ARN) that specifies the resource across
-- services.
--
-- 'name', 'resourceInfo_name' - The name of the resource.
--
-- 'resourceDetails', 'resourceInfo_resourceDetails' - The details related to the resource.
--
-- 'resourceType', 'resourceInfo_resourceType' - Provides information about the Service Catalog App Registry resource
-- type.
newResourceInfo ::
  ResourceInfo
newResourceInfo =
  ResourceInfo'
    { arn = Prelude.Nothing,
      name = Prelude.Nothing,
      resourceDetails = Prelude.Nothing,
      resourceType = Prelude.Nothing
    }

-- | The Amazon resource name (ARN) that specifies the resource across
-- services.
resourceInfo_arn :: Lens.Lens' ResourceInfo (Prelude.Maybe Prelude.Text)
resourceInfo_arn = Lens.lens (\ResourceInfo' {arn} -> arn) (\s@ResourceInfo' {} a -> s {arn = a} :: ResourceInfo)

-- | The name of the resource.
resourceInfo_name :: Lens.Lens' ResourceInfo (Prelude.Maybe Prelude.Text)
resourceInfo_name = Lens.lens (\ResourceInfo' {name} -> name) (\s@ResourceInfo' {} a -> s {name = a} :: ResourceInfo)

-- | The details related to the resource.
resourceInfo_resourceDetails :: Lens.Lens' ResourceInfo (Prelude.Maybe ResourceDetails)
resourceInfo_resourceDetails = Lens.lens (\ResourceInfo' {resourceDetails} -> resourceDetails) (\s@ResourceInfo' {} a -> s {resourceDetails = a} :: ResourceInfo)

-- | Provides information about the Service Catalog App Registry resource
-- type.
resourceInfo_resourceType :: Lens.Lens' ResourceInfo (Prelude.Maybe ResourceType)
resourceInfo_resourceType = Lens.lens (\ResourceInfo' {resourceType} -> resourceType) (\s@ResourceInfo' {} a -> s {resourceType = a} :: ResourceInfo)

instance Data.FromJSON ResourceInfo where
  parseJSON =
    Data.withObject
      "ResourceInfo"
      ( \x ->
          ResourceInfo'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "resourceDetails")
            Prelude.<*> (x Data..:? "resourceType")
      )

instance Prelude.Hashable ResourceInfo where
  hashWithSalt _salt ResourceInfo' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` resourceDetails
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData ResourceInfo where
  rnf ResourceInfo' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf resourceDetails
      `Prelude.seq` Prelude.rnf resourceType
