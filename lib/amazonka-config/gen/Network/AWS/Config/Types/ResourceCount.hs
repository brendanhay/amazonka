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
    resResourceType,
    resCount,
  )
where

import Network.AWS.Config.Types.ResourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object that contains the resource type and the number of resources.
--
-- /See:/ 'mkResourceCount' smart constructor.
data ResourceCount = ResourceCount'
  { resourceType ::
      Lude.Maybe ResourceType,
    count :: Lude.Maybe Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceCount' with the minimum fields required to make a request.
--
-- * 'count' - The number of resources.
-- * 'resourceType' - The resource type (for example, @"AWS::EC2::Instance"@ ).
mkResourceCount ::
  ResourceCount
mkResourceCount =
  ResourceCount' {resourceType = Lude.Nothing, count = Lude.Nothing}

-- | The resource type (for example, @"AWS::EC2::Instance"@ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
resResourceType :: Lens.Lens' ResourceCount (Lude.Maybe ResourceType)
resResourceType = Lens.lens (resourceType :: ResourceCount -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: ResourceCount)
{-# DEPRECATED resResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The number of resources.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
resCount :: Lens.Lens' ResourceCount (Lude.Maybe Lude.Integer)
resCount = Lens.lens (count :: ResourceCount -> Lude.Maybe Lude.Integer) (\s a -> s {count = a} :: ResourceCount)
{-# DEPRECATED resCount "Use generic-lens or generic-optics with 'count' instead." #-}

instance Lude.FromJSON ResourceCount where
  parseJSON =
    Lude.withObject
      "ResourceCount"
      ( \x ->
          ResourceCount'
            Lude.<$> (x Lude..:? "resourceType") Lude.<*> (x Lude..:? "count")
      )
