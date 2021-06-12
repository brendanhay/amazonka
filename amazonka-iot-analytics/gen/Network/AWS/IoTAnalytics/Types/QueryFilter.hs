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
-- Module      : Network.AWS.IoTAnalytics.Types.QueryFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.QueryFilter where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types.DeltaTime
import qualified Network.AWS.Lens as Lens

-- | Information that is used to filter message data, to segregate it
-- according to the timeframe in which it arrives.
--
-- /See:/ 'newQueryFilter' smart constructor.
data QueryFilter = QueryFilter'
  { -- | Used to limit data to that which has arrived since the last execution of
    -- the action.
    deltaTime :: Core.Maybe DeltaTime
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  QueryFilter' {deltaTime = Core.Nothing}

-- | Used to limit data to that which has arrived since the last execution of
-- the action.
queryFilter_deltaTime :: Lens.Lens' QueryFilter (Core.Maybe DeltaTime)
queryFilter_deltaTime = Lens.lens (\QueryFilter' {deltaTime} -> deltaTime) (\s@QueryFilter' {} a -> s {deltaTime = a} :: QueryFilter)

instance Core.FromJSON QueryFilter where
  parseJSON =
    Core.withObject
      "QueryFilter"
      ( \x ->
          QueryFilter' Core.<$> (x Core..:? "deltaTime")
      )

instance Core.Hashable QueryFilter

instance Core.NFData QueryFilter

instance Core.ToJSON QueryFilter where
  toJSON QueryFilter' {..} =
    Core.object
      ( Core.catMaybes
          [("deltaTime" Core..=) Core.<$> deltaTime]
      )
