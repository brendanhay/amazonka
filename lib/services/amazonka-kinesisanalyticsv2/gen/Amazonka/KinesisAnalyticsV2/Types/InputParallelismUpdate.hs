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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.InputParallelismUpdate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.InputParallelismUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | For a SQL-based Kinesis Data Analytics application, provides updates to
-- the parallelism count.
--
-- /See:/ 'newInputParallelismUpdate' smart constructor.
data InputParallelismUpdate = InputParallelismUpdate'
  { -- | The number of in-application streams to create for the specified
    -- streaming source.
    countUpdate :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputParallelismUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'countUpdate', 'inputParallelismUpdate_countUpdate' - The number of in-application streams to create for the specified
-- streaming source.
newInputParallelismUpdate ::
  -- | 'countUpdate'
  Prelude.Natural ->
  InputParallelismUpdate
newInputParallelismUpdate pCountUpdate_ =
  InputParallelismUpdate'
    { countUpdate =
        pCountUpdate_
    }

-- | The number of in-application streams to create for the specified
-- streaming source.
inputParallelismUpdate_countUpdate :: Lens.Lens' InputParallelismUpdate Prelude.Natural
inputParallelismUpdate_countUpdate = Lens.lens (\InputParallelismUpdate' {countUpdate} -> countUpdate) (\s@InputParallelismUpdate' {} a -> s {countUpdate = a} :: InputParallelismUpdate)

instance Prelude.Hashable InputParallelismUpdate where
  hashWithSalt _salt InputParallelismUpdate' {..} =
    _salt `Prelude.hashWithSalt` countUpdate

instance Prelude.NFData InputParallelismUpdate where
  rnf InputParallelismUpdate' {..} =
    Prelude.rnf countUpdate

instance Data.ToJSON InputParallelismUpdate where
  toJSON InputParallelismUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("CountUpdate" Data..= countUpdate)]
      )
