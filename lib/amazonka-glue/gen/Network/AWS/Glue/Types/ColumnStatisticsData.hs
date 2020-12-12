{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ColumnStatisticsData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ColumnStatisticsData
  ( ColumnStatisticsData (..),

    -- * Smart constructor
    mkColumnStatisticsData,

    -- * Lenses
    csdBinaryColumnStatisticsData,
    csdDateColumnStatisticsData,
    csdBooleanColumnStatisticsData,
    csdDecimalColumnStatisticsData,
    csdDoubleColumnStatisticsData,
    csdStringColumnStatisticsData,
    csdLongColumnStatisticsData,
    csdType,
  )
where

import Network.AWS.Glue.Types.BinaryColumnStatisticsData
import Network.AWS.Glue.Types.BooleanColumnStatisticsData
import Network.AWS.Glue.Types.ColumnStatisticsType
import Network.AWS.Glue.Types.DateColumnStatisticsData
import Network.AWS.Glue.Types.DecimalColumnStatisticsData
import Network.AWS.Glue.Types.DoubleColumnStatisticsData
import Network.AWS.Glue.Types.LongColumnStatisticsData
import Network.AWS.Glue.Types.StringColumnStatisticsData
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the individual types of column statistics data. Only one data object should be set and indicated by the @Type@ attribute.
--
-- /See:/ 'mkColumnStatisticsData' smart constructor.
data ColumnStatisticsData = ColumnStatisticsData'
  { binaryColumnStatisticsData ::
      Lude.Maybe BinaryColumnStatisticsData,
    dateColumnStatisticsData ::
      Lude.Maybe DateColumnStatisticsData,
    booleanColumnStatisticsData ::
      Lude.Maybe BooleanColumnStatisticsData,
    decimalColumnStatisticsData ::
      Lude.Maybe DecimalColumnStatisticsData,
    doubleColumnStatisticsData ::
      Lude.Maybe DoubleColumnStatisticsData,
    stringColumnStatisticsData ::
      Lude.Maybe StringColumnStatisticsData,
    longColumnStatisticsData ::
      Lude.Maybe LongColumnStatisticsData,
    type' :: ColumnStatisticsType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ColumnStatisticsData' with the minimum fields required to make a request.
--
-- * 'binaryColumnStatisticsData' - Binary column statistics data.
-- * 'booleanColumnStatisticsData' - Boolean column statistics data.
-- * 'dateColumnStatisticsData' - Date column statistics data.
-- * 'decimalColumnStatisticsData' - Decimal column statistics data.
-- * 'doubleColumnStatisticsData' - Double column statistics data.
-- * 'longColumnStatisticsData' - Long column statistics data.
-- * 'stringColumnStatisticsData' - String column statistics data.
-- * 'type'' - The type of column statistics data.
mkColumnStatisticsData ::
  -- | 'type''
  ColumnStatisticsType ->
  ColumnStatisticsData
mkColumnStatisticsData pType_ =
  ColumnStatisticsData'
    { binaryColumnStatisticsData = Lude.Nothing,
      dateColumnStatisticsData = Lude.Nothing,
      booleanColumnStatisticsData = Lude.Nothing,
      decimalColumnStatisticsData = Lude.Nothing,
      doubleColumnStatisticsData = Lude.Nothing,
      stringColumnStatisticsData = Lude.Nothing,
      longColumnStatisticsData = Lude.Nothing,
      type' = pType_
    }

-- | Binary column statistics data.
--
-- /Note:/ Consider using 'binaryColumnStatisticsData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdBinaryColumnStatisticsData :: Lens.Lens' ColumnStatisticsData (Lude.Maybe BinaryColumnStatisticsData)
csdBinaryColumnStatisticsData = Lens.lens (binaryColumnStatisticsData :: ColumnStatisticsData -> Lude.Maybe BinaryColumnStatisticsData) (\s a -> s {binaryColumnStatisticsData = a} :: ColumnStatisticsData)
{-# DEPRECATED csdBinaryColumnStatisticsData "Use generic-lens or generic-optics with 'binaryColumnStatisticsData' instead." #-}

-- | Date column statistics data.
--
-- /Note:/ Consider using 'dateColumnStatisticsData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdDateColumnStatisticsData :: Lens.Lens' ColumnStatisticsData (Lude.Maybe DateColumnStatisticsData)
csdDateColumnStatisticsData = Lens.lens (dateColumnStatisticsData :: ColumnStatisticsData -> Lude.Maybe DateColumnStatisticsData) (\s a -> s {dateColumnStatisticsData = a} :: ColumnStatisticsData)
{-# DEPRECATED csdDateColumnStatisticsData "Use generic-lens or generic-optics with 'dateColumnStatisticsData' instead." #-}

-- | Boolean column statistics data.
--
-- /Note:/ Consider using 'booleanColumnStatisticsData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdBooleanColumnStatisticsData :: Lens.Lens' ColumnStatisticsData (Lude.Maybe BooleanColumnStatisticsData)
csdBooleanColumnStatisticsData = Lens.lens (booleanColumnStatisticsData :: ColumnStatisticsData -> Lude.Maybe BooleanColumnStatisticsData) (\s a -> s {booleanColumnStatisticsData = a} :: ColumnStatisticsData)
{-# DEPRECATED csdBooleanColumnStatisticsData "Use generic-lens or generic-optics with 'booleanColumnStatisticsData' instead." #-}

-- | Decimal column statistics data.
--
-- /Note:/ Consider using 'decimalColumnStatisticsData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdDecimalColumnStatisticsData :: Lens.Lens' ColumnStatisticsData (Lude.Maybe DecimalColumnStatisticsData)
csdDecimalColumnStatisticsData = Lens.lens (decimalColumnStatisticsData :: ColumnStatisticsData -> Lude.Maybe DecimalColumnStatisticsData) (\s a -> s {decimalColumnStatisticsData = a} :: ColumnStatisticsData)
{-# DEPRECATED csdDecimalColumnStatisticsData "Use generic-lens or generic-optics with 'decimalColumnStatisticsData' instead." #-}

-- | Double column statistics data.
--
-- /Note:/ Consider using 'doubleColumnStatisticsData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdDoubleColumnStatisticsData :: Lens.Lens' ColumnStatisticsData (Lude.Maybe DoubleColumnStatisticsData)
csdDoubleColumnStatisticsData = Lens.lens (doubleColumnStatisticsData :: ColumnStatisticsData -> Lude.Maybe DoubleColumnStatisticsData) (\s a -> s {doubleColumnStatisticsData = a} :: ColumnStatisticsData)
{-# DEPRECATED csdDoubleColumnStatisticsData "Use generic-lens or generic-optics with 'doubleColumnStatisticsData' instead." #-}

-- | String column statistics data.
--
-- /Note:/ Consider using 'stringColumnStatisticsData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdStringColumnStatisticsData :: Lens.Lens' ColumnStatisticsData (Lude.Maybe StringColumnStatisticsData)
csdStringColumnStatisticsData = Lens.lens (stringColumnStatisticsData :: ColumnStatisticsData -> Lude.Maybe StringColumnStatisticsData) (\s a -> s {stringColumnStatisticsData = a} :: ColumnStatisticsData)
{-# DEPRECATED csdStringColumnStatisticsData "Use generic-lens or generic-optics with 'stringColumnStatisticsData' instead." #-}

-- | Long column statistics data.
--
-- /Note:/ Consider using 'longColumnStatisticsData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdLongColumnStatisticsData :: Lens.Lens' ColumnStatisticsData (Lude.Maybe LongColumnStatisticsData)
csdLongColumnStatisticsData = Lens.lens (longColumnStatisticsData :: ColumnStatisticsData -> Lude.Maybe LongColumnStatisticsData) (\s a -> s {longColumnStatisticsData = a} :: ColumnStatisticsData)
{-# DEPRECATED csdLongColumnStatisticsData "Use generic-lens or generic-optics with 'longColumnStatisticsData' instead." #-}

-- | The type of column statistics data.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdType :: Lens.Lens' ColumnStatisticsData ColumnStatisticsType
csdType = Lens.lens (type' :: ColumnStatisticsData -> ColumnStatisticsType) (\s a -> s {type' = a} :: ColumnStatisticsData)
{-# DEPRECATED csdType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON ColumnStatisticsData where
  parseJSON =
    Lude.withObject
      "ColumnStatisticsData"
      ( \x ->
          ColumnStatisticsData'
            Lude.<$> (x Lude..:? "BinaryColumnStatisticsData")
            Lude.<*> (x Lude..:? "DateColumnStatisticsData")
            Lude.<*> (x Lude..:? "BooleanColumnStatisticsData")
            Lude.<*> (x Lude..:? "DecimalColumnStatisticsData")
            Lude.<*> (x Lude..:? "DoubleColumnStatisticsData")
            Lude.<*> (x Lude..:? "StringColumnStatisticsData")
            Lude.<*> (x Lude..:? "LongColumnStatisticsData")
            Lude.<*> (x Lude..: "Type")
      )

instance Lude.ToJSON ColumnStatisticsData where
  toJSON ColumnStatisticsData' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("BinaryColumnStatisticsData" Lude..=)
              Lude.<$> binaryColumnStatisticsData,
            ("DateColumnStatisticsData" Lude..=)
              Lude.<$> dateColumnStatisticsData,
            ("BooleanColumnStatisticsData" Lude..=)
              Lude.<$> booleanColumnStatisticsData,
            ("DecimalColumnStatisticsData" Lude..=)
              Lude.<$> decimalColumnStatisticsData,
            ("DoubleColumnStatisticsData" Lude..=)
              Lude.<$> doubleColumnStatisticsData,
            ("StringColumnStatisticsData" Lude..=)
              Lude.<$> stringColumnStatisticsData,
            ("LongColumnStatisticsData" Lude..=)
              Lude.<$> longColumnStatisticsData,
            Lude.Just ("Type" Lude..= type')
          ]
      )
