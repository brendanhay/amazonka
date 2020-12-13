{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatasetEntry
  ( DatasetEntry (..),

    -- * Smart constructor
    mkDatasetEntry,

    -- * Lenses
    deEntryName,
    deDataURI,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The reference to a data set entry.
--
-- /See:/ 'mkDatasetEntry' smart constructor.
data DatasetEntry = DatasetEntry'
  { -- | The name of the data set item.
    entryName :: Lude.Maybe Lude.Text,
    -- | The presigned URI of the data set item.
    dataURI :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DatasetEntry' with the minimum fields required to make a request.
--
-- * 'entryName' - The name of the data set item.
-- * 'dataURI' - The presigned URI of the data set item.
mkDatasetEntry ::
  DatasetEntry
mkDatasetEntry =
  DatasetEntry' {entryName = Lude.Nothing, dataURI = Lude.Nothing}

-- | The name of the data set item.
--
-- /Note:/ Consider using 'entryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deEntryName :: Lens.Lens' DatasetEntry (Lude.Maybe Lude.Text)
deEntryName = Lens.lens (entryName :: DatasetEntry -> Lude.Maybe Lude.Text) (\s a -> s {entryName = a} :: DatasetEntry)
{-# DEPRECATED deEntryName "Use generic-lens or generic-optics with 'entryName' instead." #-}

-- | The presigned URI of the data set item.
--
-- /Note:/ Consider using 'dataURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deDataURI :: Lens.Lens' DatasetEntry (Lude.Maybe Lude.Text)
deDataURI = Lens.lens (dataURI :: DatasetEntry -> Lude.Maybe Lude.Text) (\s a -> s {dataURI = a} :: DatasetEntry)
{-# DEPRECATED deDataURI "Use generic-lens or generic-optics with 'dataURI' instead." #-}

instance Lude.FromJSON DatasetEntry where
  parseJSON =
    Lude.withObject
      "DatasetEntry"
      ( \x ->
          DatasetEntry'
            Lude.<$> (x Lude..:? "entryName") Lude.<*> (x Lude..:? "dataURI")
      )
