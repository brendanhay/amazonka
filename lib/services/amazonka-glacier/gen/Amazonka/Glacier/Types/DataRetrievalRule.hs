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
-- Module      : Amazonka.Glacier.Types.DataRetrievalRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glacier.Types.DataRetrievalRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Data retrieval policy rule.
--
-- /See:/ 'newDataRetrievalRule' smart constructor.
data DataRetrievalRule = DataRetrievalRule'
  { -- | The maximum number of bytes that can be retrieved in an hour.
    --
    -- This field is required only if the value of the Strategy field is
    -- @BytesPerHour@. Your PUT operation will be rejected if the Strategy
    -- field is not set to @BytesPerHour@ and you set this field.
    bytesPerHour :: Prelude.Maybe Prelude.Integer,
    -- | The type of data retrieval policy to set.
    --
    -- Valid values: BytesPerHour|FreeTier|None
    strategy :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataRetrievalRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bytesPerHour', 'dataRetrievalRule_bytesPerHour' - The maximum number of bytes that can be retrieved in an hour.
--
-- This field is required only if the value of the Strategy field is
-- @BytesPerHour@. Your PUT operation will be rejected if the Strategy
-- field is not set to @BytesPerHour@ and you set this field.
--
-- 'strategy', 'dataRetrievalRule_strategy' - The type of data retrieval policy to set.
--
-- Valid values: BytesPerHour|FreeTier|None
newDataRetrievalRule ::
  DataRetrievalRule
newDataRetrievalRule =
  DataRetrievalRule'
    { bytesPerHour = Prelude.Nothing,
      strategy = Prelude.Nothing
    }

-- | The maximum number of bytes that can be retrieved in an hour.
--
-- This field is required only if the value of the Strategy field is
-- @BytesPerHour@. Your PUT operation will be rejected if the Strategy
-- field is not set to @BytesPerHour@ and you set this field.
dataRetrievalRule_bytesPerHour :: Lens.Lens' DataRetrievalRule (Prelude.Maybe Prelude.Integer)
dataRetrievalRule_bytesPerHour = Lens.lens (\DataRetrievalRule' {bytesPerHour} -> bytesPerHour) (\s@DataRetrievalRule' {} a -> s {bytesPerHour = a} :: DataRetrievalRule)

-- | The type of data retrieval policy to set.
--
-- Valid values: BytesPerHour|FreeTier|None
dataRetrievalRule_strategy :: Lens.Lens' DataRetrievalRule (Prelude.Maybe Prelude.Text)
dataRetrievalRule_strategy = Lens.lens (\DataRetrievalRule' {strategy} -> strategy) (\s@DataRetrievalRule' {} a -> s {strategy = a} :: DataRetrievalRule)

instance Data.FromJSON DataRetrievalRule where
  parseJSON =
    Data.withObject
      "DataRetrievalRule"
      ( \x ->
          DataRetrievalRule'
            Prelude.<$> (x Data..:? "BytesPerHour")
            Prelude.<*> (x Data..:? "Strategy")
      )

instance Prelude.Hashable DataRetrievalRule where
  hashWithSalt _salt DataRetrievalRule' {..} =
    _salt
      `Prelude.hashWithSalt` bytesPerHour
      `Prelude.hashWithSalt` strategy

instance Prelude.NFData DataRetrievalRule where
  rnf DataRetrievalRule' {..} =
    Prelude.rnf bytesPerHour `Prelude.seq`
      Prelude.rnf strategy

instance Data.ToJSON DataRetrievalRule where
  toJSON DataRetrievalRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BytesPerHour" Data..=) Prelude.<$> bytesPerHour,
            ("Strategy" Data..=) Prelude.<$> strategy
          ]
      )
