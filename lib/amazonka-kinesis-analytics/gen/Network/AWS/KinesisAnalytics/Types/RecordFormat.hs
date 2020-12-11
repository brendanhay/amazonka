-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.RecordFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.RecordFormat
  ( RecordFormat (..),

    -- * Smart constructor
    mkRecordFormat,

    -- * Lenses
    rfMappingParameters,
    rfRecordFormatType,
  )
where

import Network.AWS.KinesisAnalytics.Types.MappingParameters
import Network.AWS.KinesisAnalytics.Types.RecordFormatType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the record format and relevant mapping information that should be applied to schematize the records on the stream.
--
-- /See:/ 'mkRecordFormat' smart constructor.
data RecordFormat = RecordFormat'
  { mappingParameters ::
      Lude.Maybe MappingParameters,
    recordFormatType :: RecordFormatType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RecordFormat' with the minimum fields required to make a request.
--
-- * 'mappingParameters' - When configuring application input at the time of creating or updating an application, provides additional mapping information specific to the record format (such as JSON, CSV, or record fields delimited by some delimiter) on the streaming source.
-- * 'recordFormatType' - The type of record format.
mkRecordFormat ::
  -- | 'recordFormatType'
  RecordFormatType ->
  RecordFormat
mkRecordFormat pRecordFormatType_ =
  RecordFormat'
    { mappingParameters = Lude.Nothing,
      recordFormatType = pRecordFormatType_
    }

-- | When configuring application input at the time of creating or updating an application, provides additional mapping information specific to the record format (such as JSON, CSV, or record fields delimited by some delimiter) on the streaming source.
--
-- /Note:/ Consider using 'mappingParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfMappingParameters :: Lens.Lens' RecordFormat (Lude.Maybe MappingParameters)
rfMappingParameters = Lens.lens (mappingParameters :: RecordFormat -> Lude.Maybe MappingParameters) (\s a -> s {mappingParameters = a} :: RecordFormat)
{-# DEPRECATED rfMappingParameters "Use generic-lens or generic-optics with 'mappingParameters' instead." #-}

-- | The type of record format.
--
-- /Note:/ Consider using 'recordFormatType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfRecordFormatType :: Lens.Lens' RecordFormat RecordFormatType
rfRecordFormatType = Lens.lens (recordFormatType :: RecordFormat -> RecordFormatType) (\s a -> s {recordFormatType = a} :: RecordFormat)
{-# DEPRECATED rfRecordFormatType "Use generic-lens or generic-optics with 'recordFormatType' instead." #-}

instance Lude.FromJSON RecordFormat where
  parseJSON =
    Lude.withObject
      "RecordFormat"
      ( \x ->
          RecordFormat'
            Lude.<$> (x Lude..:? "MappingParameters")
            Lude.<*> (x Lude..: "RecordFormatType")
      )

instance Lude.ToJSON RecordFormat where
  toJSON RecordFormat' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MappingParameters" Lude..=) Lude.<$> mappingParameters,
            Lude.Just ("RecordFormatType" Lude..= recordFormatType)
          ]
      )
