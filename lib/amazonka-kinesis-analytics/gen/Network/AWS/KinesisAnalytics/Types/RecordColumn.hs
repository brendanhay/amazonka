{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.RecordColumn
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.RecordColumn
  ( RecordColumn (..),

    -- * Smart constructor
    mkRecordColumn,

    -- * Lenses
    rcSqlType,
    rcMapping,
    rcName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the mapping of each data element in the streaming source to the corresponding column in the in-application stream.
--
-- Also used to describe the format of the reference data source.
--
-- /See:/ 'mkRecordColumn' smart constructor.
data RecordColumn = RecordColumn'
  { -- | Type of column created in the in-application input stream or reference table.
    sqlType :: Lude.Text,
    -- | Reference to the data element in the streaming input or the reference data source. This element is required if the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_RecordFormat.html#analytics-Type-RecordFormat-RecordFormatTypel RecordFormatType> is @JSON@ .
    mapping :: Lude.Maybe Lude.Text,
    -- | Name of the column created in the in-application input stream or reference table.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RecordColumn' with the minimum fields required to make a request.
--
-- * 'sqlType' - Type of column created in the in-application input stream or reference table.
-- * 'mapping' - Reference to the data element in the streaming input or the reference data source. This element is required if the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_RecordFormat.html#analytics-Type-RecordFormat-RecordFormatTypel RecordFormatType> is @JSON@ .
-- * 'name' - Name of the column created in the in-application input stream or reference table.
mkRecordColumn ::
  -- | 'sqlType'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  RecordColumn
mkRecordColumn pSqlType_ pName_ =
  RecordColumn'
    { sqlType = pSqlType_,
      mapping = Lude.Nothing,
      name = pName_
    }

-- | Type of column created in the in-application input stream or reference table.
--
-- /Note:/ Consider using 'sqlType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcSqlType :: Lens.Lens' RecordColumn Lude.Text
rcSqlType = Lens.lens (sqlType :: RecordColumn -> Lude.Text) (\s a -> s {sqlType = a} :: RecordColumn)
{-# DEPRECATED rcSqlType "Use generic-lens or generic-optics with 'sqlType' instead." #-}

-- | Reference to the data element in the streaming input or the reference data source. This element is required if the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_RecordFormat.html#analytics-Type-RecordFormat-RecordFormatTypel RecordFormatType> is @JSON@ .
--
-- /Note:/ Consider using 'mapping' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcMapping :: Lens.Lens' RecordColumn (Lude.Maybe Lude.Text)
rcMapping = Lens.lens (mapping :: RecordColumn -> Lude.Maybe Lude.Text) (\s a -> s {mapping = a} :: RecordColumn)
{-# DEPRECATED rcMapping "Use generic-lens or generic-optics with 'mapping' instead." #-}

-- | Name of the column created in the in-application input stream or reference table.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcName :: Lens.Lens' RecordColumn Lude.Text
rcName = Lens.lens (name :: RecordColumn -> Lude.Text) (\s a -> s {name = a} :: RecordColumn)
{-# DEPRECATED rcName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON RecordColumn where
  parseJSON =
    Lude.withObject
      "RecordColumn"
      ( \x ->
          RecordColumn'
            Lude.<$> (x Lude..: "SqlType")
            Lude.<*> (x Lude..:? "Mapping")
            Lude.<*> (x Lude..: "Name")
      )

instance Lude.ToJSON RecordColumn where
  toJSON RecordColumn' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SqlType" Lude..= sqlType),
            ("Mapping" Lude..=) Lude.<$> mapping,
            Lude.Just ("Name" Lude..= name)
          ]
      )
