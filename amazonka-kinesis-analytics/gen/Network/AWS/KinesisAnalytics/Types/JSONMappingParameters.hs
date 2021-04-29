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
-- Module      : Network.AWS.KinesisAnalytics.Types.JSONMappingParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.JSONMappingParameters where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides additional mapping information when JSON is the record format
-- on the streaming source.
--
-- /See:/ 'newJSONMappingParameters' smart constructor.
data JSONMappingParameters = JSONMappingParameters'
  { -- | Path to the top-level parent that contains the records.
    recordRowPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'JSONMappingParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recordRowPath', 'jSONMappingParameters_recordRowPath' - Path to the top-level parent that contains the records.
newJSONMappingParameters ::
  -- | 'recordRowPath'
  Prelude.Text ->
  JSONMappingParameters
newJSONMappingParameters pRecordRowPath_ =
  JSONMappingParameters'
    { recordRowPath =
        pRecordRowPath_
    }

-- | Path to the top-level parent that contains the records.
jSONMappingParameters_recordRowPath :: Lens.Lens' JSONMappingParameters Prelude.Text
jSONMappingParameters_recordRowPath = Lens.lens (\JSONMappingParameters' {recordRowPath} -> recordRowPath) (\s@JSONMappingParameters' {} a -> s {recordRowPath = a} :: JSONMappingParameters)

instance Prelude.FromJSON JSONMappingParameters where
  parseJSON =
    Prelude.withObject
      "JSONMappingParameters"
      ( \x ->
          JSONMappingParameters'
            Prelude.<$> (x Prelude..: "RecordRowPath")
      )

instance Prelude.Hashable JSONMappingParameters

instance Prelude.NFData JSONMappingParameters

instance Prelude.ToJSON JSONMappingParameters where
  toJSON JSONMappingParameters' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("RecordRowPath" Prelude..= recordRowPath)
          ]
      )
