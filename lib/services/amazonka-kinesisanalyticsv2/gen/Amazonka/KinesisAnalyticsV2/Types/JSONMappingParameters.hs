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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.JSONMappingParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.JSONMappingParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | For a SQL-based Kinesis Data Analytics application, provides additional
-- mapping information when JSON is the record format on the streaming
-- source.
--
-- /See:/ 'newJSONMappingParameters' smart constructor.
data JSONMappingParameters = JSONMappingParameters'
  { -- | The path to the top-level parent that contains the records.
    recordRowPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JSONMappingParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recordRowPath', 'jSONMappingParameters_recordRowPath' - The path to the top-level parent that contains the records.
newJSONMappingParameters ::
  -- | 'recordRowPath'
  Prelude.Text ->
  JSONMappingParameters
newJSONMappingParameters pRecordRowPath_ =
  JSONMappingParameters'
    { recordRowPath =
        pRecordRowPath_
    }

-- | The path to the top-level parent that contains the records.
jSONMappingParameters_recordRowPath :: Lens.Lens' JSONMappingParameters Prelude.Text
jSONMappingParameters_recordRowPath = Lens.lens (\JSONMappingParameters' {recordRowPath} -> recordRowPath) (\s@JSONMappingParameters' {} a -> s {recordRowPath = a} :: JSONMappingParameters)

instance Data.FromJSON JSONMappingParameters where
  parseJSON =
    Data.withObject
      "JSONMappingParameters"
      ( \x ->
          JSONMappingParameters'
            Prelude.<$> (x Data..: "RecordRowPath")
      )

instance Prelude.Hashable JSONMappingParameters where
  hashWithSalt _salt JSONMappingParameters' {..} =
    _salt `Prelude.hashWithSalt` recordRowPath

instance Prelude.NFData JSONMappingParameters where
  rnf JSONMappingParameters' {..} =
    Prelude.rnf recordRowPath

instance Data.ToJSON JSONMappingParameters where
  toJSON JSONMappingParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("RecordRowPath" Data..= recordRowPath)
          ]
      )
