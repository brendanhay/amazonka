{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.DestinationSchema
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.DestinationSchema where

import qualified Network.AWS.Core as Core
import Network.AWS.KinesisAnalytics.Types.RecordFormatType
import qualified Network.AWS.Lens as Lens

-- | Describes the data format when records are written to the destination.
-- For more information, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output>.
--
-- /See:/ 'newDestinationSchema' smart constructor.
data DestinationSchema = DestinationSchema'
  { -- | Specifies the format of the records on the output stream.
    recordFormatType :: RecordFormatType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DestinationSchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recordFormatType', 'destinationSchema_recordFormatType' - Specifies the format of the records on the output stream.
newDestinationSchema ::
  -- | 'recordFormatType'
  RecordFormatType ->
  DestinationSchema
newDestinationSchema pRecordFormatType_ =
  DestinationSchema'
    { recordFormatType =
        pRecordFormatType_
    }

-- | Specifies the format of the records on the output stream.
destinationSchema_recordFormatType :: Lens.Lens' DestinationSchema RecordFormatType
destinationSchema_recordFormatType = Lens.lens (\DestinationSchema' {recordFormatType} -> recordFormatType) (\s@DestinationSchema' {} a -> s {recordFormatType = a} :: DestinationSchema)

instance Core.FromJSON DestinationSchema where
  parseJSON =
    Core.withObject
      "DestinationSchema"
      ( \x ->
          DestinationSchema'
            Core.<$> (x Core..: "RecordFormatType")
      )

instance Core.Hashable DestinationSchema

instance Core.NFData DestinationSchema

instance Core.ToJSON DestinationSchema where
  toJSON DestinationSchema' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("RecordFormatType" Core..= recordFormatType)
          ]
      )
