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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides additional mapping information when JSON is the record format
-- on the streaming source.
--
-- /See:/ 'newJSONMappingParameters' smart constructor.
data JSONMappingParameters = JSONMappingParameters'
  { -- | Path to the top-level parent that contains the records.
    recordRowPath :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  JSONMappingParameters
newJSONMappingParameters pRecordRowPath_ =
  JSONMappingParameters'
    { recordRowPath =
        pRecordRowPath_
    }

-- | Path to the top-level parent that contains the records.
jSONMappingParameters_recordRowPath :: Lens.Lens' JSONMappingParameters Core.Text
jSONMappingParameters_recordRowPath = Lens.lens (\JSONMappingParameters' {recordRowPath} -> recordRowPath) (\s@JSONMappingParameters' {} a -> s {recordRowPath = a} :: JSONMappingParameters)

instance Core.FromJSON JSONMappingParameters where
  parseJSON =
    Core.withObject
      "JSONMappingParameters"
      ( \x ->
          JSONMappingParameters'
            Core.<$> (x Core..: "RecordRowPath")
      )

instance Core.Hashable JSONMappingParameters

instance Core.NFData JSONMappingParameters

instance Core.ToJSON JSONMappingParameters where
  toJSON JSONMappingParameters' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("RecordRowPath" Core..= recordRowPath)]
      )
