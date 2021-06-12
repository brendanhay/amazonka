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
-- Module      : Network.AWS.IoT.Types.RelatedResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.RelatedResource where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.ResourceIdentifier
import Network.AWS.IoT.Types.ResourceType
import qualified Network.AWS.Lens as Lens

-- | Information about a related resource.
--
-- /See:/ 'newRelatedResource' smart constructor.
data RelatedResource = RelatedResource'
  { -- | Other information about the resource.
    additionalInfo :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The type of resource.
    resourceType :: Core.Maybe ResourceType,
    -- | Information that identifies the resource.
    resourceIdentifier :: Core.Maybe ResourceIdentifier
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RelatedResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalInfo', 'relatedResource_additionalInfo' - Other information about the resource.
--
-- 'resourceType', 'relatedResource_resourceType' - The type of resource.
--
-- 'resourceIdentifier', 'relatedResource_resourceIdentifier' - Information that identifies the resource.
newRelatedResource ::
  RelatedResource
newRelatedResource =
  RelatedResource'
    { additionalInfo = Core.Nothing,
      resourceType = Core.Nothing,
      resourceIdentifier = Core.Nothing
    }

-- | Other information about the resource.
relatedResource_additionalInfo :: Lens.Lens' RelatedResource (Core.Maybe (Core.HashMap Core.Text Core.Text))
relatedResource_additionalInfo = Lens.lens (\RelatedResource' {additionalInfo} -> additionalInfo) (\s@RelatedResource' {} a -> s {additionalInfo = a} :: RelatedResource) Core.. Lens.mapping Lens._Coerce

-- | The type of resource.
relatedResource_resourceType :: Lens.Lens' RelatedResource (Core.Maybe ResourceType)
relatedResource_resourceType = Lens.lens (\RelatedResource' {resourceType} -> resourceType) (\s@RelatedResource' {} a -> s {resourceType = a} :: RelatedResource)

-- | Information that identifies the resource.
relatedResource_resourceIdentifier :: Lens.Lens' RelatedResource (Core.Maybe ResourceIdentifier)
relatedResource_resourceIdentifier = Lens.lens (\RelatedResource' {resourceIdentifier} -> resourceIdentifier) (\s@RelatedResource' {} a -> s {resourceIdentifier = a} :: RelatedResource)

instance Core.FromJSON RelatedResource where
  parseJSON =
    Core.withObject
      "RelatedResource"
      ( \x ->
          RelatedResource'
            Core.<$> (x Core..:? "additionalInfo" Core..!= Core.mempty)
            Core.<*> (x Core..:? "resourceType")
            Core.<*> (x Core..:? "resourceIdentifier")
      )

instance Core.Hashable RelatedResource

instance Core.NFData RelatedResource
