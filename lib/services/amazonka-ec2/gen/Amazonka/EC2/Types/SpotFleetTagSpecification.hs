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
-- Module      : Amazonka.EC2.Types.SpotFleetTagSpecification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SpotFleetTagSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ResourceType
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | The tags for a Spot Fleet resource.
--
-- /See:/ 'newSpotFleetTagSpecification' smart constructor.
data SpotFleetTagSpecification = SpotFleetTagSpecification'
  { -- | The tags.
    tags :: Prelude.Maybe [Tag],
    -- | The type of resource. Currently, the only resource type that is
    -- supported is @instance@. To tag the Spot Fleet request on creation, use
    -- the @TagSpecifications@ parameter in @ SpotFleetRequestConfigData @.
    resourceType :: Prelude.Maybe ResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SpotFleetTagSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'spotFleetTagSpecification_tags' - The tags.
--
-- 'resourceType', 'spotFleetTagSpecification_resourceType' - The type of resource. Currently, the only resource type that is
-- supported is @instance@. To tag the Spot Fleet request on creation, use
-- the @TagSpecifications@ parameter in @ SpotFleetRequestConfigData @.
newSpotFleetTagSpecification ::
  SpotFleetTagSpecification
newSpotFleetTagSpecification =
  SpotFleetTagSpecification'
    { tags = Prelude.Nothing,
      resourceType = Prelude.Nothing
    }

-- | The tags.
spotFleetTagSpecification_tags :: Lens.Lens' SpotFleetTagSpecification (Prelude.Maybe [Tag])
spotFleetTagSpecification_tags = Lens.lens (\SpotFleetTagSpecification' {tags} -> tags) (\s@SpotFleetTagSpecification' {} a -> s {tags = a} :: SpotFleetTagSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The type of resource. Currently, the only resource type that is
-- supported is @instance@. To tag the Spot Fleet request on creation, use
-- the @TagSpecifications@ parameter in @ SpotFleetRequestConfigData @.
spotFleetTagSpecification_resourceType :: Lens.Lens' SpotFleetTagSpecification (Prelude.Maybe ResourceType)
spotFleetTagSpecification_resourceType = Lens.lens (\SpotFleetTagSpecification' {resourceType} -> resourceType) (\s@SpotFleetTagSpecification' {} a -> s {resourceType = a} :: SpotFleetTagSpecification)

instance Core.FromXML SpotFleetTagSpecification where
  parseXML x =
    SpotFleetTagSpecification'
      Prelude.<$> ( x Core..@? "tag" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "resourceType")

instance Prelude.Hashable SpotFleetTagSpecification where
  hashWithSalt _salt SpotFleetTagSpecification' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData SpotFleetTagSpecification where
  rnf SpotFleetTagSpecification' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf resourceType

instance Core.ToQuery SpotFleetTagSpecification where
  toQuery SpotFleetTagSpecification' {..} =
    Prelude.mconcat
      [ Core.toQuery
          (Core.toQueryList "Tag" Prelude.<$> tags),
        "ResourceType" Core.=: resourceType
      ]
