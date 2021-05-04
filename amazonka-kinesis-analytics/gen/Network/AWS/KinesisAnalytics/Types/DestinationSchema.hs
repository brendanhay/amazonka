{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.KinesisAnalytics.Types.RecordFormatType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the data format when records are written to the destination.
-- For more information, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output>.
--
-- /See:/ 'newDestinationSchema' smart constructor.
data DestinationSchema = DestinationSchema'
  { -- | Specifies the format of the records on the output stream.
    recordFormatType :: RecordFormatType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON DestinationSchema where
  parseJSON =
    Prelude.withObject
      "DestinationSchema"
      ( \x ->
          DestinationSchema'
            Prelude.<$> (x Prelude..: "RecordFormatType")
      )

instance Prelude.Hashable DestinationSchema

instance Prelude.NFData DestinationSchema

instance Prelude.ToJSON DestinationSchema where
  toJSON DestinationSchema' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("RecordFormatType" Prelude..= recordFormatType)
          ]
      )
