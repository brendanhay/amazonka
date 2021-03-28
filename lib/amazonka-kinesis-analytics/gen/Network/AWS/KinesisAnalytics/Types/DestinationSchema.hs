{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.DestinationSchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.DestinationSchema
  ( DestinationSchema (..)
  -- * Smart constructor
  , mkDestinationSchema
  -- * Lenses
  , dsRecordFormatType
  ) where

import qualified Network.AWS.KinesisAnalytics.Types.RecordFormatType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the data format when records are written to the destination. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output> . 
--
-- /See:/ 'mkDestinationSchema' smart constructor.
newtype DestinationSchema = DestinationSchema'
  { recordFormatType :: Types.RecordFormatType
    -- ^ Specifies the format of the records on the output stream.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DestinationSchema' value with any optional fields omitted.
mkDestinationSchema
    :: Types.RecordFormatType -- ^ 'recordFormatType'
    -> DestinationSchema
mkDestinationSchema recordFormatType
  = DestinationSchema'{recordFormatType}

-- | Specifies the format of the records on the output stream.
--
-- /Note:/ Consider using 'recordFormatType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsRecordFormatType :: Lens.Lens' DestinationSchema Types.RecordFormatType
dsRecordFormatType = Lens.field @"recordFormatType"
{-# INLINEABLE dsRecordFormatType #-}
{-# DEPRECATED recordFormatType "Use generic-lens or generic-optics with 'recordFormatType' instead"  #-}

instance Core.FromJSON DestinationSchema where
        toJSON DestinationSchema{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("RecordFormatType" Core..= recordFormatType)])

instance Core.FromJSON DestinationSchema where
        parseJSON
          = Core.withObject "DestinationSchema" Core.$
              \ x -> DestinationSchema' Core.<$> (x Core..: "RecordFormatType")
