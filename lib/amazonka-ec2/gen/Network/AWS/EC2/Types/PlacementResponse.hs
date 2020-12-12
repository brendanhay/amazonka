{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PlacementResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PlacementResponse
  ( PlacementResponse (..),

    -- * Smart constructor
    mkPlacementResponse,

    -- * Lenses
    pGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the placement of an instance.
--
-- /See:/ 'mkPlacementResponse' smart constructor.
newtype PlacementResponse = PlacementResponse'
  { groupName ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PlacementResponse' with the minimum fields required to make a request.
--
-- * 'groupName' - The name of the placement group that the instance is in.
mkPlacementResponse ::
  PlacementResponse
mkPlacementResponse = PlacementResponse' {groupName = Lude.Nothing}

-- | The name of the placement group that the instance is in.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pGroupName :: Lens.Lens' PlacementResponse (Lude.Maybe Lude.Text)
pGroupName = Lens.lens (groupName :: PlacementResponse -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: PlacementResponse)
{-# DEPRECATED pGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Lude.FromXML PlacementResponse where
  parseXML x = PlacementResponse' Lude.<$> (x Lude..@? "groupName")
