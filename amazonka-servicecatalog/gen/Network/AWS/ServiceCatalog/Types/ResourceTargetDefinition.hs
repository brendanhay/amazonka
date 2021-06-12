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
-- Module      : Network.AWS.ServiceCatalog.Types.ResourceTargetDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ResourceTargetDefinition where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.ServiceCatalog.Types.RequiresRecreation
import Network.AWS.ServiceCatalog.Types.ResourceAttribute

-- | Information about a change to a resource attribute.
--
-- /See:/ 'newResourceTargetDefinition' smart constructor.
data ResourceTargetDefinition = ResourceTargetDefinition'
  { -- | If the attribute is @Properties@, indicates whether a change to this
    -- property causes the resource to be re-created.
    requiresRecreation :: Core.Maybe RequiresRecreation,
    -- | If the attribute is @Properties@, the value is the name of the property.
    -- Otherwise, the value is null.
    name :: Core.Maybe Core.Text,
    -- | The attribute to be changed.
    attribute :: Core.Maybe ResourceAttribute
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResourceTargetDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requiresRecreation', 'resourceTargetDefinition_requiresRecreation' - If the attribute is @Properties@, indicates whether a change to this
-- property causes the resource to be re-created.
--
-- 'name', 'resourceTargetDefinition_name' - If the attribute is @Properties@, the value is the name of the property.
-- Otherwise, the value is null.
--
-- 'attribute', 'resourceTargetDefinition_attribute' - The attribute to be changed.
newResourceTargetDefinition ::
  ResourceTargetDefinition
newResourceTargetDefinition =
  ResourceTargetDefinition'
    { requiresRecreation =
        Core.Nothing,
      name = Core.Nothing,
      attribute = Core.Nothing
    }

-- | If the attribute is @Properties@, indicates whether a change to this
-- property causes the resource to be re-created.
resourceTargetDefinition_requiresRecreation :: Lens.Lens' ResourceTargetDefinition (Core.Maybe RequiresRecreation)
resourceTargetDefinition_requiresRecreation = Lens.lens (\ResourceTargetDefinition' {requiresRecreation} -> requiresRecreation) (\s@ResourceTargetDefinition' {} a -> s {requiresRecreation = a} :: ResourceTargetDefinition)

-- | If the attribute is @Properties@, the value is the name of the property.
-- Otherwise, the value is null.
resourceTargetDefinition_name :: Lens.Lens' ResourceTargetDefinition (Core.Maybe Core.Text)
resourceTargetDefinition_name = Lens.lens (\ResourceTargetDefinition' {name} -> name) (\s@ResourceTargetDefinition' {} a -> s {name = a} :: ResourceTargetDefinition)

-- | The attribute to be changed.
resourceTargetDefinition_attribute :: Lens.Lens' ResourceTargetDefinition (Core.Maybe ResourceAttribute)
resourceTargetDefinition_attribute = Lens.lens (\ResourceTargetDefinition' {attribute} -> attribute) (\s@ResourceTargetDefinition' {} a -> s {attribute = a} :: ResourceTargetDefinition)

instance Core.FromJSON ResourceTargetDefinition where
  parseJSON =
    Core.withObject
      "ResourceTargetDefinition"
      ( \x ->
          ResourceTargetDefinition'
            Core.<$> (x Core..:? "RequiresRecreation")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Attribute")
      )

instance Core.Hashable ResourceTargetDefinition

instance Core.NFData ResourceTargetDefinition
