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
-- Module      : Network.AWS.IoT.Types.TimestreamDimension
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TimestreamDimension where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Metadata attributes of the time series that are written in each measure
-- record.
--
-- /See:/ 'newTimestreamDimension' smart constructor.
data TimestreamDimension = TimestreamDimension'
  { -- | The metadata dimension name. This is the name of the column in the
    -- Amazon Timestream database table record.
    --
    -- Dimensions cannot be named: @measure_name@, @measure_value@, or @time@.
    -- These names are reserved. Dimension names cannot start with @ts_@ or
    -- @measure_value@ and they cannot contain the colon (@:@) character.
    name :: Prelude.Text,
    -- | The value to write in this column of the database record.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TimestreamDimension' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'timestreamDimension_name' - The metadata dimension name. This is the name of the column in the
-- Amazon Timestream database table record.
--
-- Dimensions cannot be named: @measure_name@, @measure_value@, or @time@.
-- These names are reserved. Dimension names cannot start with @ts_@ or
-- @measure_value@ and they cannot contain the colon (@:@) character.
--
-- 'value', 'timestreamDimension_value' - The value to write in this column of the database record.
newTimestreamDimension ::
  -- | 'name'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  TimestreamDimension
newTimestreamDimension pName_ pValue_ =
  TimestreamDimension'
    { name = pName_,
      value = pValue_
    }

-- | The metadata dimension name. This is the name of the column in the
-- Amazon Timestream database table record.
--
-- Dimensions cannot be named: @measure_name@, @measure_value@, or @time@.
-- These names are reserved. Dimension names cannot start with @ts_@ or
-- @measure_value@ and they cannot contain the colon (@:@) character.
timestreamDimension_name :: Lens.Lens' TimestreamDimension Prelude.Text
timestreamDimension_name = Lens.lens (\TimestreamDimension' {name} -> name) (\s@TimestreamDimension' {} a -> s {name = a} :: TimestreamDimension)

-- | The value to write in this column of the database record.
timestreamDimension_value :: Lens.Lens' TimestreamDimension Prelude.Text
timestreamDimension_value = Lens.lens (\TimestreamDimension' {value} -> value) (\s@TimestreamDimension' {} a -> s {value = a} :: TimestreamDimension)

instance Prelude.FromJSON TimestreamDimension where
  parseJSON =
    Prelude.withObject
      "TimestreamDimension"
      ( \x ->
          TimestreamDimension'
            Prelude.<$> (x Prelude..: "name")
            Prelude.<*> (x Prelude..: "value")
      )

instance Prelude.Hashable TimestreamDimension

instance Prelude.NFData TimestreamDimension

instance Prelude.ToJSON TimestreamDimension where
  toJSON TimestreamDimension' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Prelude..= name),
            Prelude.Just ("value" Prelude..= value)
          ]
      )
