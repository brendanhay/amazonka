-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkCardInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkCardInfo
  ( NetworkCardInfo (..),

    -- * Smart constructor
    mkNetworkCardInfo,

    -- * Lenses
    nciMaximumNetworkInterfaces,
    nciNetworkPerformance,
    nciNetworkCardIndex,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the network card support of the instance type.
--
-- /See:/ 'mkNetworkCardInfo' smart constructor.
data NetworkCardInfo = NetworkCardInfo'
  { maximumNetworkInterfaces ::
      Lude.Maybe Lude.Int,
    networkPerformance :: Lude.Maybe Lude.Text,
    networkCardIndex :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NetworkCardInfo' with the minimum fields required to make a request.
--
-- * 'maximumNetworkInterfaces' - The maximum number of network interfaces for the network card.
-- * 'networkCardIndex' - The index of the network card.
-- * 'networkPerformance' - The network performance of the network card.
mkNetworkCardInfo ::
  NetworkCardInfo
mkNetworkCardInfo =
  NetworkCardInfo'
    { maximumNetworkInterfaces = Lude.Nothing,
      networkPerformance = Lude.Nothing,
      networkCardIndex = Lude.Nothing
    }

-- | The maximum number of network interfaces for the network card.
--
-- /Note:/ Consider using 'maximumNetworkInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nciMaximumNetworkInterfaces :: Lens.Lens' NetworkCardInfo (Lude.Maybe Lude.Int)
nciMaximumNetworkInterfaces = Lens.lens (maximumNetworkInterfaces :: NetworkCardInfo -> Lude.Maybe Lude.Int) (\s a -> s {maximumNetworkInterfaces = a} :: NetworkCardInfo)
{-# DEPRECATED nciMaximumNetworkInterfaces "Use generic-lens or generic-optics with 'maximumNetworkInterfaces' instead." #-}

-- | The network performance of the network card.
--
-- /Note:/ Consider using 'networkPerformance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nciNetworkPerformance :: Lens.Lens' NetworkCardInfo (Lude.Maybe Lude.Text)
nciNetworkPerformance = Lens.lens (networkPerformance :: NetworkCardInfo -> Lude.Maybe Lude.Text) (\s a -> s {networkPerformance = a} :: NetworkCardInfo)
{-# DEPRECATED nciNetworkPerformance "Use generic-lens or generic-optics with 'networkPerformance' instead." #-}

-- | The index of the network card.
--
-- /Note:/ Consider using 'networkCardIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nciNetworkCardIndex :: Lens.Lens' NetworkCardInfo (Lude.Maybe Lude.Int)
nciNetworkCardIndex = Lens.lens (networkCardIndex :: NetworkCardInfo -> Lude.Maybe Lude.Int) (\s a -> s {networkCardIndex = a} :: NetworkCardInfo)
{-# DEPRECATED nciNetworkCardIndex "Use generic-lens or generic-optics with 'networkCardIndex' instead." #-}

instance Lude.FromXML NetworkCardInfo where
  parseXML x =
    NetworkCardInfo'
      Lude.<$> (x Lude..@? "maximumNetworkInterfaces")
      Lude.<*> (x Lude..@? "networkPerformance")
      Lude.<*> (x Lude..@? "networkCardIndex")
