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
-- Module      : Amazonka.GroundStation.Types.DataflowEndpointListItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.DataflowEndpointListItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Item in a list of @DataflowEndpoint@ groups.
--
-- /See:/ 'newDataflowEndpointListItem' smart constructor.
data DataflowEndpointListItem = DataflowEndpointListItem'
  { -- | ARN of a dataflow endpoint group.
    dataflowEndpointGroupArn :: Prelude.Maybe Prelude.Text,
    -- | UUID of a dataflow endpoint group.
    dataflowEndpointGroupId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataflowEndpointListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataflowEndpointGroupArn', 'dataflowEndpointListItem_dataflowEndpointGroupArn' - ARN of a dataflow endpoint group.
--
-- 'dataflowEndpointGroupId', 'dataflowEndpointListItem_dataflowEndpointGroupId' - UUID of a dataflow endpoint group.
newDataflowEndpointListItem ::
  DataflowEndpointListItem
newDataflowEndpointListItem =
  DataflowEndpointListItem'
    { dataflowEndpointGroupArn =
        Prelude.Nothing,
      dataflowEndpointGroupId = Prelude.Nothing
    }

-- | ARN of a dataflow endpoint group.
dataflowEndpointListItem_dataflowEndpointGroupArn :: Lens.Lens' DataflowEndpointListItem (Prelude.Maybe Prelude.Text)
dataflowEndpointListItem_dataflowEndpointGroupArn = Lens.lens (\DataflowEndpointListItem' {dataflowEndpointGroupArn} -> dataflowEndpointGroupArn) (\s@DataflowEndpointListItem' {} a -> s {dataflowEndpointGroupArn = a} :: DataflowEndpointListItem)

-- | UUID of a dataflow endpoint group.
dataflowEndpointListItem_dataflowEndpointGroupId :: Lens.Lens' DataflowEndpointListItem (Prelude.Maybe Prelude.Text)
dataflowEndpointListItem_dataflowEndpointGroupId = Lens.lens (\DataflowEndpointListItem' {dataflowEndpointGroupId} -> dataflowEndpointGroupId) (\s@DataflowEndpointListItem' {} a -> s {dataflowEndpointGroupId = a} :: DataflowEndpointListItem)

instance Data.FromJSON DataflowEndpointListItem where
  parseJSON =
    Data.withObject
      "DataflowEndpointListItem"
      ( \x ->
          DataflowEndpointListItem'
            Prelude.<$> (x Data..:? "dataflowEndpointGroupArn")
            Prelude.<*> (x Data..:? "dataflowEndpointGroupId")
      )

instance Prelude.Hashable DataflowEndpointListItem where
  hashWithSalt _salt DataflowEndpointListItem' {..} =
    _salt
      `Prelude.hashWithSalt` dataflowEndpointGroupArn
      `Prelude.hashWithSalt` dataflowEndpointGroupId

instance Prelude.NFData DataflowEndpointListItem where
  rnf DataflowEndpointListItem' {..} =
    Prelude.rnf dataflowEndpointGroupArn `Prelude.seq`
      Prelude.rnf dataflowEndpointGroupId
