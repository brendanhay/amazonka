-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetContentDeliveryRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatasetContentDeliveryRule
  ( DatasetContentDeliveryRule (..),

    -- * Smart constructor
    mkDatasetContentDeliveryRule,

    -- * Lenses
    dcdrEntryName,
    dcdrDestination,
  )
where

import Network.AWS.IoTAnalytics.Types.DatasetContentDeliveryDestination
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | When dataset contents are created, they are delivered to destination specified here.
--
-- /See:/ 'mkDatasetContentDeliveryRule' smart constructor.
data DatasetContentDeliveryRule = DatasetContentDeliveryRule'
  { entryName ::
      Lude.Maybe Lude.Text,
    destination ::
      DatasetContentDeliveryDestination
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DatasetContentDeliveryRule' with the minimum fields required to make a request.
--
-- * 'destination' - The destination to which dataset contents are delivered.
-- * 'entryName' - The name of the dataset content delivery rules entry.
mkDatasetContentDeliveryRule ::
  -- | 'destination'
  DatasetContentDeliveryDestination ->
  DatasetContentDeliveryRule
mkDatasetContentDeliveryRule pDestination_ =
  DatasetContentDeliveryRule'
    { entryName = Lude.Nothing,
      destination = pDestination_
    }

-- | The name of the dataset content delivery rules entry.
--
-- /Note:/ Consider using 'entryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcdrEntryName :: Lens.Lens' DatasetContentDeliveryRule (Lude.Maybe Lude.Text)
dcdrEntryName = Lens.lens (entryName :: DatasetContentDeliveryRule -> Lude.Maybe Lude.Text) (\s a -> s {entryName = a} :: DatasetContentDeliveryRule)
{-# DEPRECATED dcdrEntryName "Use generic-lens or generic-optics with 'entryName' instead." #-}

-- | The destination to which dataset contents are delivered.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcdrDestination :: Lens.Lens' DatasetContentDeliveryRule DatasetContentDeliveryDestination
dcdrDestination = Lens.lens (destination :: DatasetContentDeliveryRule -> DatasetContentDeliveryDestination) (\s a -> s {destination = a} :: DatasetContentDeliveryRule)
{-# DEPRECATED dcdrDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

instance Lude.FromJSON DatasetContentDeliveryRule where
  parseJSON =
    Lude.withObject
      "DatasetContentDeliveryRule"
      ( \x ->
          DatasetContentDeliveryRule'
            Lude.<$> (x Lude..:? "entryName") Lude.<*> (x Lude..: "destination")
      )

instance Lude.ToJSON DatasetContentDeliveryRule where
  toJSON DatasetContentDeliveryRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("entryName" Lude..=) Lude.<$> entryName,
            Lude.Just ("destination" Lude..= destination)
          ]
      )
