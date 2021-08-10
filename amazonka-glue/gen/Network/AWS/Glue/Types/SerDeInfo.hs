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
import qualified Network.AWS.Prelude as Prelude

-- | Information about a serialization\/deserialization program (SerDe) that
-- serves as an extractor and loader.
--
-- /See:/ 'newSerDeInfo' smart constructor.
data SerDeInfo = SerDeInfo'
  { -- | Usually the class that implements the SerDe. An example is
    -- @org.apache.hadoop.hive.serde2.columnar.ColumnarSerDe@.
    serializationLibrary :: Prelude.Maybe Prelude.Text,
    -- | Name of the SerDe.
    name :: Prelude.Maybe Prelude.Text,
    -- | These key-value pairs define initialization parameters for the SerDe.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { serializationLibrary = Prelude.Nothing,
      name = Prelude.Nothing,
      parameters = Prelude.Nothing
    }

-- | Usually the class that implements the SerDe. An example is
-- @org.apache.hadoop.hive.serde2.columnar.ColumnarSerDe@.
serDeInfo_serializationLibrary :: Lens.Lens' SerDeInfo (Prelude.Maybe Prelude.Text)
serDeInfo_serializationLibrary = Lens.lens (\SerDeInfo' {serializationLibrary} -> serializationLibrary) (\s@SerDeInfo' {} a -> s {serializationLibrary = a} :: SerDeInfo)

-- | Name of the SerDe.
serDeInfo_name :: Lens.Lens' SerDeInfo (Prelude.Maybe Prelude.Text)
serDeInfo_name = Lens.lens (\SerDeInfo' {name} -> name) (\s@SerDeInfo' {} a -> s {name = a} :: SerDeInfo)

-- | These key-value pairs define initialization parameters for the SerDe.
serDeInfo_parameters :: Lens.Lens' SerDeInfo (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
serDeInfo_parameters = Lens.lens (\SerDeInfo' {parameters} -> parameters) (\s@SerDeInfo' {} a -> s {parameters = a} :: SerDeInfo) Prelude.. Lens.mapping Lens._Coerce

instance Core.FromJSON SerDeInfo where
  parseJSON =
    Core.withObject
      "SerDeInfo"
      ( \x ->
          SerDeInfo'
            Prelude.<$> (x Core..:? "SerializationLibrary")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Parameters" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable SerDeInfo

instance Prelude.NFData SerDeInfo

instance Core.ToJSON SerDeInfo where
  toJSON SerDeInfo' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SerializationLibrary" Core..=)
              Prelude.<$> serializationLibrary,
            ("Name" Core..=) Prelude.<$> name,
            ("Parameters" Core..=) Prelude.<$> parameters
          ]
      )
