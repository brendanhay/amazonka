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
-- Module      : Amazonka.TimeStreamQuery.Types.TargetDestination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamQuery.Types.TargetDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TimeStreamQuery.Types.TimestreamDestination

-- | Destination details to write data for a target data source. Current
-- supported data source is Timestream.
--
-- /See:/ 'newTargetDestination' smart constructor.
data TargetDestination = TargetDestination'
  { -- | Query result destination details for Timestream data source.
    timestreamDestination :: Prelude.Maybe TimestreamDestination
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timestreamDestination', 'targetDestination_timestreamDestination' - Query result destination details for Timestream data source.
newTargetDestination ::
  TargetDestination
newTargetDestination =
  TargetDestination'
    { timestreamDestination =
        Prelude.Nothing
    }

-- | Query result destination details for Timestream data source.
targetDestination_timestreamDestination :: Lens.Lens' TargetDestination (Prelude.Maybe TimestreamDestination)
targetDestination_timestreamDestination = Lens.lens (\TargetDestination' {timestreamDestination} -> timestreamDestination) (\s@TargetDestination' {} a -> s {timestreamDestination = a} :: TargetDestination)

instance Data.FromJSON TargetDestination where
  parseJSON =
    Data.withObject
      "TargetDestination"
      ( \x ->
          TargetDestination'
            Prelude.<$> (x Data..:? "TimestreamDestination")
      )

instance Prelude.Hashable TargetDestination where
  hashWithSalt _salt TargetDestination' {..} =
    _salt `Prelude.hashWithSalt` timestreamDestination

instance Prelude.NFData TargetDestination where
  rnf TargetDestination' {..} =
    Prelude.rnf timestreamDestination
