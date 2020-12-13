{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotFleetTagSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotFleetTagSpecification
  ( SpotFleetTagSpecification (..),

    -- * Smart constructor
    mkSpotFleetTagSpecification,

    -- * Lenses
    sftsResourceType,
    sftsTags,
  )
where

import Network.AWS.EC2.Types.ResourceType
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The tags for a Spot Fleet resource.
--
-- /See:/ 'mkSpotFleetTagSpecification' smart constructor.
data SpotFleetTagSpecification = SpotFleetTagSpecification'
  { -- | The type of resource. Currently, the only resource type that is supported is @instance@ . To tag the Spot Fleet request on creation, use the @TagSpecifications@ parameter in <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SpotFleetRequestConfigData.html @SpotFleetRequestConfigData@ > .
    resourceType :: Lude.Maybe ResourceType,
    -- | The tags.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SpotFleetTagSpecification' with the minimum fields required to make a request.
--
-- * 'resourceType' - The type of resource. Currently, the only resource type that is supported is @instance@ . To tag the Spot Fleet request on creation, use the @TagSpecifications@ parameter in <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SpotFleetRequestConfigData.html @SpotFleetRequestConfigData@ > .
-- * 'tags' - The tags.
mkSpotFleetTagSpecification ::
  SpotFleetTagSpecification
mkSpotFleetTagSpecification =
  SpotFleetTagSpecification'
    { resourceType = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The type of resource. Currently, the only resource type that is supported is @instance@ . To tag the Spot Fleet request on creation, use the @TagSpecifications@ parameter in <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SpotFleetRequestConfigData.html @SpotFleetRequestConfigData@ > .
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sftsResourceType :: Lens.Lens' SpotFleetTagSpecification (Lude.Maybe ResourceType)
sftsResourceType = Lens.lens (resourceType :: SpotFleetTagSpecification -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: SpotFleetTagSpecification)
{-# DEPRECATED sftsResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sftsTags :: Lens.Lens' SpotFleetTagSpecification (Lude.Maybe [Tag])
sftsTags = Lens.lens (tags :: SpotFleetTagSpecification -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: SpotFleetTagSpecification)
{-# DEPRECATED sftsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML SpotFleetTagSpecification where
  parseXML x =
    SpotFleetTagSpecification'
      Lude.<$> (x Lude..@? "resourceType")
      Lude.<*> ( x Lude..@? "tag" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )

instance Lude.ToQuery SpotFleetTagSpecification where
  toQuery SpotFleetTagSpecification' {..} =
    Lude.mconcat
      [ "ResourceType" Lude.=: resourceType,
        Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags)
      ]
