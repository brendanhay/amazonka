{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.DestinationSchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.DestinationSchema
  ( DestinationSchema (..),

    -- * Smart constructor
    mkDestinationSchema,

    -- * Lenses
    dsRecordFormatType,
  )
where

import Network.AWS.KinesisAnalytics.Types.RecordFormatType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the data format when records are written to the destination. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output> .
--
-- /See:/ 'mkDestinationSchema' smart constructor.
newtype DestinationSchema = DestinationSchema'
  { -- | Specifies the format of the records on the output stream.
    recordFormatType :: RecordFormatType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DestinationSchema' with the minimum fields required to make a request.
--
-- * 'recordFormatType' - Specifies the format of the records on the output stream.
mkDestinationSchema ::
  -- | 'recordFormatType'
  RecordFormatType ->
  DestinationSchema
mkDestinationSchema pRecordFormatType_ =
  DestinationSchema' {recordFormatType = pRecordFormatType_}

-- | Specifies the format of the records on the output stream.
--
-- /Note:/ Consider using 'recordFormatType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsRecordFormatType :: Lens.Lens' DestinationSchema RecordFormatType
dsRecordFormatType = Lens.lens (recordFormatType :: DestinationSchema -> RecordFormatType) (\s a -> s {recordFormatType = a} :: DestinationSchema)
{-# DEPRECATED dsRecordFormatType "Use generic-lens or generic-optics with 'recordFormatType' instead." #-}

instance Lude.FromJSON DestinationSchema where
  parseJSON =
    Lude.withObject
      "DestinationSchema"
      (\x -> DestinationSchema' Lude.<$> (x Lude..: "RecordFormatType"))

instance Lude.ToJSON DestinationSchema where
  toJSON DestinationSchema' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("RecordFormatType" Lude..= recordFormatType)]
      )
