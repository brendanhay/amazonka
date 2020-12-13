{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ResourceCount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ResourceCount
  ( ResourceCount (..),

    -- * Smart constructor
    mkResourceCount,

    -- * Lenses
    rcgResourceType,
    rcgCount,
  )
where

import Network.AWS.Config.Types.ResourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object that contains the resource type and the number of resources.
--
-- /See:/ 'mkResourceCount' smart constructor.
data ResourceCount = ResourceCount'
  { -- | The resource type (for example, @"AWS::EC2::Instance"@ ).
    resourceType :: Lude.Maybe ResourceType,
    -- | The number of resources.
    count :: Lude.Maybe Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceCount' with the minimum fields required to make a request.
--
-- * 'resourceType' - The resource type (for example, @"AWS::EC2::Instance"@ ).
-- * 'count' - The number of resources.
mkResourceCount ::
  ResourceCount
mkResourceCount =
  ResourceCount' {resourceType = Lude.Nothing, count = Lude.Nothing}

-- | The resource type (for example, @"AWS::EC2::Instance"@ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcgResourceType :: Lens.Lens' ResourceCount (Lude.Maybe ResourceType)
rcgResourceType = Lens.lens (resourceType :: ResourceCount -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: ResourceCount)
{-# DEPRECATED rcgResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The number of resources.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcgCount :: Lens.Lens' ResourceCount (Lude.Maybe Lude.Integer)
rcgCount = Lens.lens (count :: ResourceCount -> Lude.Maybe Lude.Integer) (\s a -> s {count = a} :: ResourceCount)
{-# DEPRECATED rcgCount "Use generic-lens or generic-optics with 'count' instead." #-}

instance Lude.FromJSON ResourceCount where
  parseJSON =
    Lude.withObject
      "ResourceCount"
      ( \x ->
          ResourceCount'
            Lude.<$> (x Lude..:? "resourceType") Lude.<*> (x Lude..:? "count")
      )
