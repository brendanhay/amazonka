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
-- Module      : Amazonka.IoTAnalytics.Types.QueryFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.QueryFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTAnalytics.Types.DeltaTime
import qualified Amazonka.Prelude as Prelude

-- | Information that is used to filter message data, to segregate it
-- according to the timeframe in which it arrives.
--
-- /See:/ 'newQueryFilter' smart constructor.
data QueryFilter = QueryFilter'
  { -- | Used to limit data to that which has arrived since the last execution of
    -- the action.
    deltaTime :: Prelude.Maybe DeltaTime
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deltaTime', 'queryFilter_deltaTime' - Used to limit data to that which has arrived since the last execution of
-- the action.
newQueryFilter ::
  QueryFilter
newQueryFilter =
  QueryFilter' {deltaTime = Prelude.Nothing}

-- | Used to limit data to that which has arrived since the last execution of
-- the action.
queryFilter_deltaTime :: Lens.Lens' QueryFilter (Prelude.Maybe DeltaTime)
queryFilter_deltaTime = Lens.lens (\QueryFilter' {deltaTime} -> deltaTime) (\s@QueryFilter' {} a -> s {deltaTime = a} :: QueryFilter)

instance Data.FromJSON QueryFilter where
  parseJSON =
    Data.withObject
      "QueryFilter"
      ( \x ->
          QueryFilter' Prelude.<$> (x Data..:? "deltaTime")
      )

instance Prelude.Hashable QueryFilter where
  hashWithSalt _salt QueryFilter' {..} =
    _salt `Prelude.hashWithSalt` deltaTime

instance Prelude.NFData QueryFilter where
  rnf QueryFilter' {..} = Prelude.rnf deltaTime

instance Data.ToJSON QueryFilter where
  toJSON QueryFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [("deltaTime" Data..=) Prelude.<$> deltaTime]
      )
