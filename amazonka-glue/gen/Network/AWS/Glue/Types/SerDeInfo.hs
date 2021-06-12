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
-- Module      : Network.AWS.Glue.Types.SerDeInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SerDeInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about a serialization\/deserialization program (SerDe) that
-- serves as an extractor and loader.
--
-- /See:/ 'newSerDeInfo' smart constructor.
data SerDeInfo = SerDeInfo'
  { -- | Usually the class that implements the SerDe. An example is
    -- @org.apache.hadoop.hive.serde2.columnar.ColumnarSerDe@.
    serializationLibrary :: Core.Maybe Core.Text,
    -- | Name of the SerDe.
    name :: Core.Maybe Core.Text,
    -- | These key-value pairs define initialization parameters for the SerDe.
    parameters :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SerDeInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serializationLibrary', 'serDeInfo_serializationLibrary' - Usually the class that implements the SerDe. An example is
-- @org.apache.hadoop.hive.serde2.columnar.ColumnarSerDe@.
--
-- 'name', 'serDeInfo_name' - Name of the SerDe.
--
-- 'parameters', 'serDeInfo_parameters' - These key-value pairs define initialization parameters for the SerDe.
newSerDeInfo ::
  SerDeInfo
newSerDeInfo =
  SerDeInfo'
    { serializationLibrary = Core.Nothing,
      name = Core.Nothing,
      parameters = Core.Nothing
    }

-- | Usually the class that implements the SerDe. An example is
-- @org.apache.hadoop.hive.serde2.columnar.ColumnarSerDe@.
serDeInfo_serializationLibrary :: Lens.Lens' SerDeInfo (Core.Maybe Core.Text)
serDeInfo_serializationLibrary = Lens.lens (\SerDeInfo' {serializationLibrary} -> serializationLibrary) (\s@SerDeInfo' {} a -> s {serializationLibrary = a} :: SerDeInfo)

-- | Name of the SerDe.
serDeInfo_name :: Lens.Lens' SerDeInfo (Core.Maybe Core.Text)
serDeInfo_name = Lens.lens (\SerDeInfo' {name} -> name) (\s@SerDeInfo' {} a -> s {name = a} :: SerDeInfo)

-- | These key-value pairs define initialization parameters for the SerDe.
serDeInfo_parameters :: Lens.Lens' SerDeInfo (Core.Maybe (Core.HashMap Core.Text Core.Text))
serDeInfo_parameters = Lens.lens (\SerDeInfo' {parameters} -> parameters) (\s@SerDeInfo' {} a -> s {parameters = a} :: SerDeInfo) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON SerDeInfo where
  parseJSON =
    Core.withObject
      "SerDeInfo"
      ( \x ->
          SerDeInfo'
            Core.<$> (x Core..:? "SerializationLibrary")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Parameters" Core..!= Core.mempty)
      )

instance Core.Hashable SerDeInfo

instance Core.NFData SerDeInfo

instance Core.ToJSON SerDeInfo where
  toJSON SerDeInfo' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SerializationLibrary" Core..=)
              Core.<$> serializationLibrary,
            ("Name" Core..=) Core.<$> name,
            ("Parameters" Core..=) Core.<$> parameters
          ]
      )
