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
-- Module      : Amazonka.SnowDeviceManagement.Types.ResourceSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SnowDeviceManagement.Types.ResourceSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A summary of a resource available on the device.
--
-- /See:/ 'newResourceSummary' smart constructor.
data ResourceSummary = ResourceSummary'
  { -- | The Amazon Resource Name (ARN) of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the resource.
    id :: Prelude.Maybe Prelude.Text,
    -- | The resource type.
    resourceType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'resourceSummary_arn' - The Amazon Resource Name (ARN) of the resource.
--
-- 'id', 'resourceSummary_id' - The ID of the resource.
--
-- 'resourceType', 'resourceSummary_resourceType' - The resource type.
newResourceSummary ::
  -- | 'resourceType'
  Prelude.Text ->
  ResourceSummary
newResourceSummary pResourceType_ =
  ResourceSummary'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      resourceType = pResourceType_
    }

-- | The Amazon Resource Name (ARN) of the resource.
resourceSummary_arn :: Lens.Lens' ResourceSummary (Prelude.Maybe Prelude.Text)
resourceSummary_arn = Lens.lens (\ResourceSummary' {arn} -> arn) (\s@ResourceSummary' {} a -> s {arn = a} :: ResourceSummary)

-- | The ID of the resource.
resourceSummary_id :: Lens.Lens' ResourceSummary (Prelude.Maybe Prelude.Text)
resourceSummary_id = Lens.lens (\ResourceSummary' {id} -> id) (\s@ResourceSummary' {} a -> s {id = a} :: ResourceSummary)

-- | The resource type.
resourceSummary_resourceType :: Lens.Lens' ResourceSummary Prelude.Text
resourceSummary_resourceType = Lens.lens (\ResourceSummary' {resourceType} -> resourceType) (\s@ResourceSummary' {} a -> s {resourceType = a} :: ResourceSummary)

instance Data.FromJSON ResourceSummary where
  parseJSON =
    Data.withObject
      "ResourceSummary"
      ( \x ->
          ResourceSummary'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..: "resourceType")
      )

instance Prelude.Hashable ResourceSummary where
  hashWithSalt _salt ResourceSummary' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData ResourceSummary where
  rnf ResourceSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf resourceType
