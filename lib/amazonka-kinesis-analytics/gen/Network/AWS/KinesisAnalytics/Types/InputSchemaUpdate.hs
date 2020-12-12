{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputSchemaUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.InputSchemaUpdate
  ( InputSchemaUpdate (..),

    -- * Smart constructor
    mkInputSchemaUpdate,

    -- * Lenses
    isuRecordFormatUpdate,
    isuRecordEncodingUpdate,
    isuRecordColumnUpdates,
  )
where

import Network.AWS.KinesisAnalytics.Types.RecordColumn
import Network.AWS.KinesisAnalytics.Types.RecordFormat
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes updates for the application's input schema.
--
-- /See:/ 'mkInputSchemaUpdate' smart constructor.
data InputSchemaUpdate = InputSchemaUpdate'
  { recordFormatUpdate ::
      Lude.Maybe RecordFormat,
    recordEncodingUpdate :: Lude.Maybe Lude.Text,
    recordColumnUpdates ::
      Lude.Maybe (Lude.NonEmpty RecordColumn)
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputSchemaUpdate' with the minimum fields required to make a request.
--
-- * 'recordColumnUpdates' - A list of @RecordColumn@ objects. Each object describes the mapping of the streaming source element to the corresponding column in the in-application stream.
-- * 'recordEncodingUpdate' - Specifies the encoding of the records in the streaming source. For example, UTF-8.
-- * 'recordFormatUpdate' - Specifies the format of the records on the streaming source.
mkInputSchemaUpdate ::
  InputSchemaUpdate
mkInputSchemaUpdate =
  InputSchemaUpdate'
    { recordFormatUpdate = Lude.Nothing,
      recordEncodingUpdate = Lude.Nothing,
      recordColumnUpdates = Lude.Nothing
    }

-- | Specifies the format of the records on the streaming source.
--
-- /Note:/ Consider using 'recordFormatUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isuRecordFormatUpdate :: Lens.Lens' InputSchemaUpdate (Lude.Maybe RecordFormat)
isuRecordFormatUpdate = Lens.lens (recordFormatUpdate :: InputSchemaUpdate -> Lude.Maybe RecordFormat) (\s a -> s {recordFormatUpdate = a} :: InputSchemaUpdate)
{-# DEPRECATED isuRecordFormatUpdate "Use generic-lens or generic-optics with 'recordFormatUpdate' instead." #-}

-- | Specifies the encoding of the records in the streaming source. For example, UTF-8.
--
-- /Note:/ Consider using 'recordEncodingUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isuRecordEncodingUpdate :: Lens.Lens' InputSchemaUpdate (Lude.Maybe Lude.Text)
isuRecordEncodingUpdate = Lens.lens (recordEncodingUpdate :: InputSchemaUpdate -> Lude.Maybe Lude.Text) (\s a -> s {recordEncodingUpdate = a} :: InputSchemaUpdate)
{-# DEPRECATED isuRecordEncodingUpdate "Use generic-lens or generic-optics with 'recordEncodingUpdate' instead." #-}

-- | A list of @RecordColumn@ objects. Each object describes the mapping of the streaming source element to the corresponding column in the in-application stream.
--
-- /Note:/ Consider using 'recordColumnUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isuRecordColumnUpdates :: Lens.Lens' InputSchemaUpdate (Lude.Maybe (Lude.NonEmpty RecordColumn))
isuRecordColumnUpdates = Lens.lens (recordColumnUpdates :: InputSchemaUpdate -> Lude.Maybe (Lude.NonEmpty RecordColumn)) (\s a -> s {recordColumnUpdates = a} :: InputSchemaUpdate)
{-# DEPRECATED isuRecordColumnUpdates "Use generic-lens or generic-optics with 'recordColumnUpdates' instead." #-}

instance Lude.ToJSON InputSchemaUpdate where
  toJSON InputSchemaUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RecordFormatUpdate" Lude..=) Lude.<$> recordFormatUpdate,
            ("RecordEncodingUpdate" Lude..=) Lude.<$> recordEncodingUpdate,
            ("RecordColumnUpdates" Lude..=) Lude.<$> recordColumnUpdates
          ]
      )
