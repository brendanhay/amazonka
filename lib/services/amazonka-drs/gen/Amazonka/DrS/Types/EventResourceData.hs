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
-- Module      : Amazonka.DrS.Types.EventResourceData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.EventResourceData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types.SourceNetworkData
import qualified Amazonka.Prelude as Prelude

-- | Properties of resource related to a job event.
--
-- /See:/ 'newEventResourceData' smart constructor.
data EventResourceData = EventResourceData'
  { -- | Source Network properties.
    sourceNetworkData :: Prelude.Maybe SourceNetworkData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventResourceData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceNetworkData', 'eventResourceData_sourceNetworkData' - Source Network properties.
newEventResourceData ::
  EventResourceData
newEventResourceData =
  EventResourceData'
    { sourceNetworkData =
        Prelude.Nothing
    }

-- | Source Network properties.
eventResourceData_sourceNetworkData :: Lens.Lens' EventResourceData (Prelude.Maybe SourceNetworkData)
eventResourceData_sourceNetworkData = Lens.lens (\EventResourceData' {sourceNetworkData} -> sourceNetworkData) (\s@EventResourceData' {} a -> s {sourceNetworkData = a} :: EventResourceData)

instance Data.FromJSON EventResourceData where
  parseJSON =
    Data.withObject
      "EventResourceData"
      ( \x ->
          EventResourceData'
            Prelude.<$> (x Data..:? "sourceNetworkData")
      )

instance Prelude.Hashable EventResourceData where
  hashWithSalt _salt EventResourceData' {..} =
    _salt `Prelude.hashWithSalt` sourceNetworkData

instance Prelude.NFData EventResourceData where
  rnf EventResourceData' {..} =
    Prelude.rnf sourceNetworkData
