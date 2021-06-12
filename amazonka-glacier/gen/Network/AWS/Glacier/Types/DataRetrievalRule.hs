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
-- Module      : Network.AWS.Glacier.Types.DataRetrievalRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.DataRetrievalRule where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Data retrieval policy rule.
--
-- /See:/ 'newDataRetrievalRule' smart constructor.
data DataRetrievalRule = DataRetrievalRule'
  { -- | The maximum number of bytes that can be retrieved in an hour.
    --
    -- This field is required only if the value of the Strategy field is
    -- @BytesPerHour@. Your PUT operation will be rejected if the Strategy
    -- field is not set to @BytesPerHour@ and you set this field.
    bytesPerHour :: Core.Maybe Core.Integer,
    -- | The type of data retrieval policy to set.
    --
    -- Valid values: BytesPerHour|FreeTier|None
    strategy :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { bytesPerHour = Core.Nothing,
      strategy = Core.Nothing
    }

-- | The maximum number of bytes that can be retrieved in an hour.
--
-- This field is required only if the value of the Strategy field is
-- @BytesPerHour@. Your PUT operation will be rejected if the Strategy
-- field is not set to @BytesPerHour@ and you set this field.
dataRetrievalRule_bytesPerHour :: Lens.Lens' DataRetrievalRule (Core.Maybe Core.Integer)
dataRetrievalRule_bytesPerHour = Lens.lens (\DataRetrievalRule' {bytesPerHour} -> bytesPerHour) (\s@DataRetrievalRule' {} a -> s {bytesPerHour = a} :: DataRetrievalRule)

-- | The type of data retrieval policy to set.
--
-- Valid values: BytesPerHour|FreeTier|None
dataRetrievalRule_strategy :: Lens.Lens' DataRetrievalRule (Core.Maybe Core.Text)
dataRetrievalRule_strategy = Lens.lens (\DataRetrievalRule' {strategy} -> strategy) (\s@DataRetrievalRule' {} a -> s {strategy = a} :: DataRetrievalRule)

instance Core.FromJSON DataRetrievalRule where
  parseJSON =
    Core.withObject
      "DataRetrievalRule"
      ( \x ->
          DataRetrievalRule'
            Core.<$> (x Core..:? "BytesPerHour")
            Core.<*> (x Core..:? "Strategy")
      )

instance Core.Hashable DataRetrievalRule

instance Core.NFData DataRetrievalRule

instance Core.ToJSON DataRetrievalRule where
  toJSON DataRetrievalRule' {..} =
    Core.object
      ( Core.catMaybes
          [ ("BytesPerHour" Core..=) Core.<$> bytesPerHour,
            ("Strategy" Core..=) Core.<$> strategy
          ]
      )
