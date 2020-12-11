-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatastoreStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatastoreStatistics
  ( DatastoreStatistics (..),

    -- * Smart constructor
    mkDatastoreStatistics,

    -- * Lenses
    dsSize,
  )
where

import Network.AWS.IoTAnalytics.Types.EstimatedResourceSize
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Statistical information about the data store.
--
-- /See:/ 'mkDatastoreStatistics' smart constructor.
newtype DatastoreStatistics = DatastoreStatistics'
  { size ::
      Lude.Maybe EstimatedResourceSize
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DatastoreStatistics' with the minimum fields required to make a request.
--
-- * 'size' - The estimated size of the data store.
mkDatastoreStatistics ::
  DatastoreStatistics
mkDatastoreStatistics = DatastoreStatistics' {size = Lude.Nothing}

-- | The estimated size of the data store.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSize :: Lens.Lens' DatastoreStatistics (Lude.Maybe EstimatedResourceSize)
dsSize = Lens.lens (size :: DatastoreStatistics -> Lude.Maybe EstimatedResourceSize) (\s a -> s {size = a} :: DatastoreStatistics)
{-# DEPRECATED dsSize "Use generic-lens or generic-optics with 'size' instead." #-}

instance Lude.FromJSON DatastoreStatistics where
  parseJSON =
    Lude.withObject
      "DatastoreStatistics"
      (\x -> DatastoreStatistics' Lude.<$> (x Lude..:? "size"))
