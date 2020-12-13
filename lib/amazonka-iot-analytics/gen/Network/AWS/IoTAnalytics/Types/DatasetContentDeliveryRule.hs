{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    dcdrDestination,
    dcdrEntryName,
  )
where

import Network.AWS.IoTAnalytics.Types.DatasetContentDeliveryDestination
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | When dataset contents are created, they are delivered to destination specified here.
--
-- /See:/ 'mkDatasetContentDeliveryRule' smart constructor.
data DatasetContentDeliveryRule = DatasetContentDeliveryRule'
  { -- | The destination to which dataset contents are delivered.
    destination :: DatasetContentDeliveryDestination,
    -- | The name of the dataset content delivery rules entry.
    entryName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
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
    { destination = pDestination_,
      entryName = Lude.Nothing
    }

-- | The destination to which dataset contents are delivered.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcdrDestination :: Lens.Lens' DatasetContentDeliveryRule DatasetContentDeliveryDestination
dcdrDestination = Lens.lens (destination :: DatasetContentDeliveryRule -> DatasetContentDeliveryDestination) (\s a -> s {destination = a} :: DatasetContentDeliveryRule)
{-# DEPRECATED dcdrDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | The name of the dataset content delivery rules entry.
--
-- /Note:/ Consider using 'entryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcdrEntryName :: Lens.Lens' DatasetContentDeliveryRule (Lude.Maybe Lude.Text)
dcdrEntryName = Lens.lens (entryName :: DatasetContentDeliveryRule -> Lude.Maybe Lude.Text) (\s a -> s {entryName = a} :: DatasetContentDeliveryRule)
{-# DEPRECATED dcdrEntryName "Use generic-lens or generic-optics with 'entryName' instead." #-}

instance Lude.FromJSON DatasetContentDeliveryRule where
  parseJSON =
    Lude.withObject
      "DatasetContentDeliveryRule"
      ( \x ->
          DatasetContentDeliveryRule'
            Lude.<$> (x Lude..: "destination") Lude.<*> (x Lude..:? "entryName")
      )

instance Lude.ToJSON DatasetContentDeliveryRule where
  toJSON DatasetContentDeliveryRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("destination" Lude..= destination),
            ("entryName" Lude..=) Lude.<$> entryName
          ]
      )
