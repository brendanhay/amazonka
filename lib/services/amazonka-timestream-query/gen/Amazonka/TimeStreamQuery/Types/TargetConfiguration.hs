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
-- Module      : Amazonka.TimeStreamQuery.Types.TargetConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamQuery.Types.TargetConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TimeStreamQuery.Types.TimestreamConfiguration

-- | Configuration used for writing the output of a query.
--
-- /See:/ 'newTargetConfiguration' smart constructor.
data TargetConfiguration = TargetConfiguration'
  { -- | Configuration needed to write data into the Timestream database and
    -- table.
    timestreamConfiguration :: TimestreamConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timestreamConfiguration', 'targetConfiguration_timestreamConfiguration' - Configuration needed to write data into the Timestream database and
-- table.
newTargetConfiguration ::
  -- | 'timestreamConfiguration'
  TimestreamConfiguration ->
  TargetConfiguration
newTargetConfiguration pTimestreamConfiguration_ =
  TargetConfiguration'
    { timestreamConfiguration =
        pTimestreamConfiguration_
    }

-- | Configuration needed to write data into the Timestream database and
-- table.
targetConfiguration_timestreamConfiguration :: Lens.Lens' TargetConfiguration TimestreamConfiguration
targetConfiguration_timestreamConfiguration = Lens.lens (\TargetConfiguration' {timestreamConfiguration} -> timestreamConfiguration) (\s@TargetConfiguration' {} a -> s {timestreamConfiguration = a} :: TargetConfiguration)

instance Data.FromJSON TargetConfiguration where
  parseJSON =
    Data.withObject
      "TargetConfiguration"
      ( \x ->
          TargetConfiguration'
            Prelude.<$> (x Data..: "TimestreamConfiguration")
      )

instance Prelude.Hashable TargetConfiguration where
  hashWithSalt _salt TargetConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` timestreamConfiguration

instance Prelude.NFData TargetConfiguration where
  rnf TargetConfiguration' {..} =
    Prelude.rnf timestreamConfiguration

instance Data.ToJSON TargetConfiguration where
  toJSON TargetConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "TimestreamConfiguration"
                  Data..= timestreamConfiguration
              )
          ]
      )
