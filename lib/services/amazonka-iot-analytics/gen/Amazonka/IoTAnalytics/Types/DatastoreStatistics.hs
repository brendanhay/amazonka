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
-- Module      : Amazonka.IoTAnalytics.Types.DatastoreStatistics
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.DatastoreStatistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTAnalytics.Types.EstimatedResourceSize
import qualified Amazonka.Prelude as Prelude

-- | Statistical information about the data store.
--
-- /See:/ 'newDatastoreStatistics' smart constructor.
data DatastoreStatistics = DatastoreStatistics'
  { -- | The estimated size of the data store.
    size :: Prelude.Maybe EstimatedResourceSize
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatastoreStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'size', 'datastoreStatistics_size' - The estimated size of the data store.
newDatastoreStatistics ::
  DatastoreStatistics
newDatastoreStatistics =
  DatastoreStatistics' {size = Prelude.Nothing}

-- | The estimated size of the data store.
datastoreStatistics_size :: Lens.Lens' DatastoreStatistics (Prelude.Maybe EstimatedResourceSize)
datastoreStatistics_size = Lens.lens (\DatastoreStatistics' {size} -> size) (\s@DatastoreStatistics' {} a -> s {size = a} :: DatastoreStatistics)

instance Core.FromJSON DatastoreStatistics where
  parseJSON =
    Core.withObject
      "DatastoreStatistics"
      ( \x ->
          DatastoreStatistics' Prelude.<$> (x Core..:? "size")
      )

instance Prelude.Hashable DatastoreStatistics where
  hashWithSalt _salt DatastoreStatistics' {..} =
    _salt `Prelude.hashWithSalt` size

instance Prelude.NFData DatastoreStatistics where
  rnf DatastoreStatistics' {..} = Prelude.rnf size
