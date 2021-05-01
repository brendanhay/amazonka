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
-- Module      : Network.AWS.Glacier.Types.DataRetrievalRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.DataRetrievalRule where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON DataRetrievalRule where
  parseJSON =
    Prelude.withObject
      "DataRetrievalRule"
      ( \x ->
          DataRetrievalRule'
            Prelude.<$> (x Prelude..:? "BytesPerHour")
            Prelude.<*> (x Prelude..:? "Strategy")
      )

instance Prelude.Hashable DataRetrievalRule

instance Prelude.NFData DataRetrievalRule

instance Prelude.ToJSON DataRetrievalRule where
  toJSON DataRetrievalRule' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("BytesPerHour" Prelude..=)
              Prelude.<$> bytesPerHour,
            ("Strategy" Prelude..=) Prelude.<$> strategy
          ]
      )
