-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PlacementGroupInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PlacementGroupInfo
  ( PlacementGroupInfo (..),

    -- * Smart constructor
    mkPlacementGroupInfo,

    -- * Lenses
    pgiSupportedStrategies,
  )
where

import Network.AWS.EC2.Types.PlacementGroupStrategy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the placement group support of the instance type.
--
-- /See:/ 'mkPlacementGroupInfo' smart constructor.
newtype PlacementGroupInfo = PlacementGroupInfo'
  { supportedStrategies ::
      Lude.Maybe [PlacementGroupStrategy]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PlacementGroupInfo' with the minimum fields required to make a request.
--
-- * 'supportedStrategies' - The supported placement group types.
mkPlacementGroupInfo ::
  PlacementGroupInfo
mkPlacementGroupInfo =
  PlacementGroupInfo' {supportedStrategies = Lude.Nothing}

-- | The supported placement group types.
--
-- /Note:/ Consider using 'supportedStrategies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgiSupportedStrategies :: Lens.Lens' PlacementGroupInfo (Lude.Maybe [PlacementGroupStrategy])
pgiSupportedStrategies = Lens.lens (supportedStrategies :: PlacementGroupInfo -> Lude.Maybe [PlacementGroupStrategy]) (\s a -> s {supportedStrategies = a} :: PlacementGroupInfo)
{-# DEPRECATED pgiSupportedStrategies "Use generic-lens or generic-optics with 'supportedStrategies' instead." #-}

instance Lude.FromXML PlacementGroupInfo where
  parseXML x =
    PlacementGroupInfo'
      Lude.<$> ( x Lude..@? "supportedStrategies" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
