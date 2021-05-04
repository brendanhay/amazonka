{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ResourceType
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The tags for a Spot Fleet resource.
--
-- /See:/ 'newSpotFleetTagSpecification' smart constructor.
data SpotFleetTagSpecification = SpotFleetTagSpecification'
  { -- | The type of resource. Currently, the only resource type that is
    -- supported is @instance@. To tag the Spot Fleet request on creation, use
    -- the @TagSpecifications@ parameter in
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SpotFleetRequestConfigData.html SpotFleetRequestConfigData>
    -- .
    resourceType :: Prelude.Maybe ResourceType,
    -- | The tags.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The type of resource. Currently, the only resource type that is
-- supported is @instance@. To tag the Spot Fleet request on creation, use
-- the @TagSpecifications@ parameter in
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SpotFleetRequestConfigData.html SpotFleetRequestConfigData>
-- .
spotFleetTagSpecification_resourceType :: Lens.Lens' SpotFleetTagSpecification (Prelude.Maybe ResourceType)
spotFleetTagSpecification_resourceType = Lens.lens (\SpotFleetTagSpecification' {resourceType} -> resourceType) (\s@SpotFleetTagSpecification' {} a -> s {resourceType = a} :: SpotFleetTagSpecification)

-- | The tags.
spotFleetTagSpecification_tags :: Lens.Lens' SpotFleetTagSpecification (Prelude.Maybe [Tag])
spotFleetTagSpecification_tags = Lens.lens (\SpotFleetTagSpecification' {tags} -> tags) (\s@SpotFleetTagSpecification' {} a -> s {tags = a} :: SpotFleetTagSpecification) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML SpotFleetTagSpecification where
  parseXML x =
    SpotFleetTagSpecification'
      Prelude.<$> (x Prelude..@? "resourceType")
      Prelude.<*> ( x Prelude..@? "tag" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )

instance Prelude.Hashable SpotFleetTagSpecification

instance Prelude.NFData SpotFleetTagSpecification

instance Prelude.ToQuery SpotFleetTagSpecification where
  toQuery SpotFleetTagSpecification' {..} =
    Prelude.mconcat
      [ "ResourceType" Prelude.=: resourceType,
        Prelude.toQuery
          (Prelude.toQueryList "Tag" Prelude.<$> tags)
      ]
