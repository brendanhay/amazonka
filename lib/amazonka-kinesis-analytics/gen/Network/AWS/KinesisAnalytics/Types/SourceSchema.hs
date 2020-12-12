{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.SourceSchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.SourceSchema
  ( SourceSchema (..),

    -- * Smart constructor
    mkSourceSchema,

    -- * Lenses
    ssRecordEncoding,
    ssRecordFormat,
    ssRecordColumns,
  )
where

import Network.AWS.KinesisAnalytics.Types.RecordColumn
import Network.AWS.KinesisAnalytics.Types.RecordFormat
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the format of the data in the streaming source, and how each data element maps to corresponding columns created in the in-application stream.
--
-- /See:/ 'mkSourceSchema' smart constructor.
data SourceSchema = SourceSchema'
  { recordEncoding ::
      Lude.Maybe Lude.Text,
    recordFormat :: RecordFormat,
    recordColumns :: Lude.NonEmpty RecordColumn
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SourceSchema' with the minimum fields required to make a request.
--
-- * 'recordColumns' - A list of @RecordColumn@ objects.
-- * 'recordEncoding' - Specifies the encoding of the records in the streaming source. For example, UTF-8.
-- * 'recordFormat' - Specifies the format of the records on the streaming source.
mkSourceSchema ::
  -- | 'recordFormat'
  RecordFormat ->
  -- | 'recordColumns'
  Lude.NonEmpty RecordColumn ->
  SourceSchema
mkSourceSchema pRecordFormat_ pRecordColumns_ =
  SourceSchema'
    { recordEncoding = Lude.Nothing,
      recordFormat = pRecordFormat_,
      recordColumns = pRecordColumns_
    }

-- | Specifies the encoding of the records in the streaming source. For example, UTF-8.
--
-- /Note:/ Consider using 'recordEncoding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssRecordEncoding :: Lens.Lens' SourceSchema (Lude.Maybe Lude.Text)
ssRecordEncoding = Lens.lens (recordEncoding :: SourceSchema -> Lude.Maybe Lude.Text) (\s a -> s {recordEncoding = a} :: SourceSchema)
{-# DEPRECATED ssRecordEncoding "Use generic-lens or generic-optics with 'recordEncoding' instead." #-}

-- | Specifies the format of the records on the streaming source.
--
-- /Note:/ Consider using 'recordFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssRecordFormat :: Lens.Lens' SourceSchema RecordFormat
ssRecordFormat = Lens.lens (recordFormat :: SourceSchema -> RecordFormat) (\s a -> s {recordFormat = a} :: SourceSchema)
{-# DEPRECATED ssRecordFormat "Use generic-lens or generic-optics with 'recordFormat' instead." #-}

-- | A list of @RecordColumn@ objects.
--
-- /Note:/ Consider using 'recordColumns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssRecordColumns :: Lens.Lens' SourceSchema (Lude.NonEmpty RecordColumn)
ssRecordColumns = Lens.lens (recordColumns :: SourceSchema -> Lude.NonEmpty RecordColumn) (\s a -> s {recordColumns = a} :: SourceSchema)
{-# DEPRECATED ssRecordColumns "Use generic-lens or generic-optics with 'recordColumns' instead." #-}

instance Lude.FromJSON SourceSchema where
  parseJSON =
    Lude.withObject
      "SourceSchema"
      ( \x ->
          SourceSchema'
            Lude.<$> (x Lude..:? "RecordEncoding")
            Lude.<*> (x Lude..: "RecordFormat")
            Lude.<*> (x Lude..: "RecordColumns")
      )

instance Lude.ToJSON SourceSchema where
  toJSON SourceSchema' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RecordEncoding" Lude..=) Lude.<$> recordEncoding,
            Lude.Just ("RecordFormat" Lude..= recordFormat),
            Lude.Just ("RecordColumns" Lude..= recordColumns)
          ]
      )
