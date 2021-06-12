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
-- Module      : Network.AWS.EC2.Types.SpotFleetTagSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotFleetTagSpecification where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ResourceType
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | The tags for a Spot Fleet resource.
--
-- /See:/ 'newSpotFleetTagSpecification' smart constructor.
data SpotFleetTagSpecification = SpotFleetTagSpecification'
  { -- | The type of resource. Currently, the only resource type that is
    -- supported is @instance@. To tag the Spot Fleet request on creation, use
    -- the @TagSpecifications@ parameter in
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SpotFleetRequestConfigData.html SpotFleetRequestConfigData>
    -- .
    resourceType :: Core.Maybe ResourceType,
    -- | The tags.
    tags :: Core.Maybe [Tag]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SpotFleetTagSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'spotFleetTagSpecification_resourceType' - The type of resource. Currently, the only resource type that is
-- supported is @instance@. To tag the Spot Fleet request on creation, use
-- the @TagSpecifications@ parameter in
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SpotFleetRequestConfigData.html SpotFleetRequestConfigData>
-- .
--
-- 'tags', 'spotFleetTagSpecification_tags' - The tags.
newSpotFleetTagSpecification ::
  SpotFleetTagSpecification
newSpotFleetTagSpecification =
  SpotFleetTagSpecification'
    { resourceType =
        Core.Nothing,
      tags = Core.Nothing
    }

-- | The type of resource. Currently, the only resource type that is
-- supported is @instance@. To tag the Spot Fleet request on creation, use
-- the @TagSpecifications@ parameter in
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SpotFleetRequestConfigData.html SpotFleetRequestConfigData>
-- .
spotFleetTagSpecification_resourceType :: Lens.Lens' SpotFleetTagSpecification (Core.Maybe ResourceType)
spotFleetTagSpecification_resourceType = Lens.lens (\SpotFleetTagSpecification' {resourceType} -> resourceType) (\s@SpotFleetTagSpecification' {} a -> s {resourceType = a} :: SpotFleetTagSpecification)

-- | The tags.
spotFleetTagSpecification_tags :: Lens.Lens' SpotFleetTagSpecification (Core.Maybe [Tag])
spotFleetTagSpecification_tags = Lens.lens (\SpotFleetTagSpecification' {tags} -> tags) (\s@SpotFleetTagSpecification' {} a -> s {tags = a} :: SpotFleetTagSpecification) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML SpotFleetTagSpecification where
  parseXML x =
    SpotFleetTagSpecification'
      Core.<$> (x Core..@? "resourceType")
      Core.<*> ( x Core..@? "tag" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )

instance Core.Hashable SpotFleetTagSpecification

instance Core.NFData SpotFleetTagSpecification

instance Core.ToQuery SpotFleetTagSpecification where
  toQuery SpotFleetTagSpecification' {..} =
    Core.mconcat
      [ "ResourceType" Core.=: resourceType,
        Core.toQuery (Core.toQueryList "Tag" Core.<$> tags)
      ]
