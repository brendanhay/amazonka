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
-- Module      : Network.AWS.EC2.Types.LaunchTemplateTagSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateTagSpecification where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ResourceType
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | The tag specification for the launch template.
--
-- /See:/ 'newLaunchTemplateTagSpecification' smart constructor.
data LaunchTemplateTagSpecification = LaunchTemplateTagSpecification'
  { -- | The type of resource.
    resourceType :: Core.Maybe ResourceType,
    -- | The tags for the resource.
    tags :: Core.Maybe [Tag]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LaunchTemplateTagSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'launchTemplateTagSpecification_resourceType' - The type of resource.
--
-- 'tags', 'launchTemplateTagSpecification_tags' - The tags for the resource.
newLaunchTemplateTagSpecification ::
  LaunchTemplateTagSpecification
newLaunchTemplateTagSpecification =
  LaunchTemplateTagSpecification'
    { resourceType =
        Core.Nothing,
      tags = Core.Nothing
    }

-- | The type of resource.
launchTemplateTagSpecification_resourceType :: Lens.Lens' LaunchTemplateTagSpecification (Core.Maybe ResourceType)
launchTemplateTagSpecification_resourceType = Lens.lens (\LaunchTemplateTagSpecification' {resourceType} -> resourceType) (\s@LaunchTemplateTagSpecification' {} a -> s {resourceType = a} :: LaunchTemplateTagSpecification)

-- | The tags for the resource.
launchTemplateTagSpecification_tags :: Lens.Lens' LaunchTemplateTagSpecification (Core.Maybe [Tag])
launchTemplateTagSpecification_tags = Lens.lens (\LaunchTemplateTagSpecification' {tags} -> tags) (\s@LaunchTemplateTagSpecification' {} a -> s {tags = a} :: LaunchTemplateTagSpecification) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML LaunchTemplateTagSpecification where
  parseXML x =
    LaunchTemplateTagSpecification'
      Core.<$> (x Core..@? "resourceType")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )

instance Core.Hashable LaunchTemplateTagSpecification

instance Core.NFData LaunchTemplateTagSpecification
