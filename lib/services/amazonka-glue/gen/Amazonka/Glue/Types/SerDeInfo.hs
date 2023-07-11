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
-- Module      : Amazonka.Glue.Types.SerDeInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.SerDeInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a serialization\/deserialization program (SerDe) that
-- serves as an extractor and loader.
--
-- /See:/ 'newSerDeInfo' smart constructor.
data SerDeInfo = SerDeInfo'
  { -- | Name of the SerDe.
    name :: Prelude.Maybe Prelude.Text,
    -- | These key-value pairs define initialization parameters for the SerDe.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Usually the class that implements the SerDe. An example is
    -- @org.apache.hadoop.hive.serde2.columnar.ColumnarSerDe@.
    serializationLibrary :: Prelude.Maybe Prelude.Text
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
-- 'name', 'serDeInfo_name' - Name of the SerDe.
--
-- 'parameters', 'serDeInfo_parameters' - These key-value pairs define initialization parameters for the SerDe.
--
-- 'serializationLibrary', 'serDeInfo_serializationLibrary' - Usually the class that implements the SerDe. An example is
-- @org.apache.hadoop.hive.serde2.columnar.ColumnarSerDe@.
newSerDeInfo ::
  SerDeInfo
newSerDeInfo =
  SerDeInfo'
    { name = Prelude.Nothing,
      parameters = Prelude.Nothing,
      serializationLibrary = Prelude.Nothing
    }

-- | Name of the SerDe.
serDeInfo_name :: Lens.Lens' SerDeInfo (Prelude.Maybe Prelude.Text)
serDeInfo_name = Lens.lens (\SerDeInfo' {name} -> name) (\s@SerDeInfo' {} a -> s {name = a} :: SerDeInfo)

-- | These key-value pairs define initialization parameters for the SerDe.
serDeInfo_parameters :: Lens.Lens' SerDeInfo (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
serDeInfo_parameters = Lens.lens (\SerDeInfo' {parameters} -> parameters) (\s@SerDeInfo' {} a -> s {parameters = a} :: SerDeInfo) Prelude.. Lens.mapping Lens.coerced

-- | Usually the class that implements the SerDe. An example is
-- @org.apache.hadoop.hive.serde2.columnar.ColumnarSerDe@.
serDeInfo_serializationLibrary :: Lens.Lens' SerDeInfo (Prelude.Maybe Prelude.Text)
serDeInfo_serializationLibrary = Lens.lens (\SerDeInfo' {serializationLibrary} -> serializationLibrary) (\s@SerDeInfo' {} a -> s {serializationLibrary = a} :: SerDeInfo)

instance Data.FromJSON SerDeInfo where
  parseJSON =
    Data.withObject
      "SerDeInfo"
      ( \x ->
          SerDeInfo'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Parameters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SerializationLibrary")
      )

instance Prelude.Hashable SerDeInfo where
  hashWithSalt _salt SerDeInfo' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` serializationLibrary

instance Prelude.NFData SerDeInfo where
  rnf SerDeInfo' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf serializationLibrary

instance Data.ToJSON SerDeInfo where
  toJSON SerDeInfo' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("Parameters" Data..=) Prelude.<$> parameters,
            ("SerializationLibrary" Data..=)
              Prelude.<$> serializationLibrary
          ]
      )
