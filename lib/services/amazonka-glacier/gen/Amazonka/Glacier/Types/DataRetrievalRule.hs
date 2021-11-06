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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glacier.Types.DataRetrievalRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Data retrieval policy rule.
--
-- /See:/ 'newDataRetrievalRule' smart constructor.
data DataRetrievalRule = DataRetrievalRule'
  { -- | The type of data retrieval policy to set.
    --
    -- Valid values: BytesPerHour|FreeTier|None
    strategy :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of bytes that can be retrieved in an hour.
    --
    -- This field is required only if the value of the Strategy field is
    -- @BytesPerHour@. Your PUT operation will be rejected if the Strategy
    -- field is not set to @BytesPerHour@ and you set this field.
    bytesPerHour :: Prelude.Maybe Prelude.Integer
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
-- 'strategy', 'dataRetrievalRule_strategy' - The type of data retrieval policy to set.
--
-- Valid values: BytesPerHour|FreeTier|None
--
-- 'bytesPerHour', 'dataRetrievalRule_bytesPerHour' - The maximum number of bytes that can be retrieved in an hour.
--
-- This field is required only if the value of the Strategy field is
-- @BytesPerHour@. Your PUT operation will be rejected if the Strategy
-- field is not set to @BytesPerHour@ and you set this field.
newDataRetrievalRule ::
  DataRetrievalRule
newDataRetrievalRule =
  DataRetrievalRule'
    { strategy = Prelude.Nothing,
      bytesPerHour = Prelude.Nothing
    }

-- | The type of data retrieval policy to set.
--
-- Valid values: BytesPerHour|FreeTier|None
dataRetrievalRule_strategy :: Lens.Lens' DataRetrievalRule (Prelude.Maybe Prelude.Text)
dataRetrievalRule_strategy = Lens.lens (\DataRetrievalRule' {strategy} -> strategy) (\s@DataRetrievalRule' {} a -> s {strategy = a} :: DataRetrievalRule)

-- | The maximum number of bytes that can be retrieved in an hour.
--
-- This field is required only if the value of the Strategy field is
-- @BytesPerHour@. Your PUT operation will be rejected if the Strategy
-- field is not set to @BytesPerHour@ and you set this field.
dataRetrievalRule_bytesPerHour :: Lens.Lens' DataRetrievalRule (Prelude.Maybe Prelude.Integer)
dataRetrievalRule_bytesPerHour = Lens.lens (\DataRetrievalRule' {bytesPerHour} -> bytesPerHour) (\s@DataRetrievalRule' {} a -> s {bytesPerHour = a} :: DataRetrievalRule)

instance Core.FromJSON DataRetrievalRule where
  parseJSON =
    Core.withObject
      "DataRetrievalRule"
      ( \x ->
          DataRetrievalRule'
            Prelude.<$> (x Core..:? "Strategy")
            Prelude.<*> (x Core..:? "BytesPerHour")
      )

instance Prelude.Hashable DataRetrievalRule

instance Prelude.NFData DataRetrievalRule

instance Core.ToJSON DataRetrievalRule where
  toJSON DataRetrievalRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Strategy" Core..=) Prelude.<$> strategy,
            ("BytesPerHour" Core..=) Prelude.<$> bytesPerHour
          ]
      )
