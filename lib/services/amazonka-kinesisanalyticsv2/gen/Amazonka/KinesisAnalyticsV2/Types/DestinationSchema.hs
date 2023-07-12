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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.DestinationSchema
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.DestinationSchema where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types.RecordFormatType
import qualified Amazonka.Prelude as Prelude

-- | Describes the data format when records are written to the destination in
-- a SQL-based Kinesis Data Analytics application.
--
-- /See:/ 'newDestinationSchema' smart constructor.
data DestinationSchema = DestinationSchema'
  { -- | Specifies the format of the records on the output stream.
    recordFormatType :: RecordFormatType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON DestinationSchema where
  parseJSON =
    Data.withObject
      "DestinationSchema"
      ( \x ->
          DestinationSchema'
            Prelude.<$> (x Data..: "RecordFormatType")
      )

instance Prelude.Hashable DestinationSchema where
  hashWithSalt _salt DestinationSchema' {..} =
    _salt `Prelude.hashWithSalt` recordFormatType

instance Prelude.NFData DestinationSchema where
  rnf DestinationSchema' {..} =
    Prelude.rnf recordFormatType

instance Data.ToJSON DestinationSchema where
  toJSON DestinationSchema' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("RecordFormatType" Data..= recordFormatType)
          ]
      )
