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
-- Module      : Amazonka.GroundStation.Types.DataflowDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.DataflowDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types.Destination
import Amazonka.GroundStation.Types.Source
import qualified Amazonka.Prelude as Prelude

-- | Information about a dataflow edge used in a contact.
--
-- /See:/ 'newDataflowDetail' smart constructor.
data DataflowDetail = DataflowDetail'
  { destination :: Prelude.Maybe Destination,
    -- | Error message for a dataflow.
    errorMessage :: Prelude.Maybe Prelude.Text,
    source :: Prelude.Maybe Source
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataflowDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'dataflowDetail_destination' - Undocumented member.
--
-- 'errorMessage', 'dataflowDetail_errorMessage' - Error message for a dataflow.
--
-- 'source', 'dataflowDetail_source' - Undocumented member.
newDataflowDetail ::
  DataflowDetail
newDataflowDetail =
  DataflowDetail'
    { destination = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      source = Prelude.Nothing
    }

-- | Undocumented member.
dataflowDetail_destination :: Lens.Lens' DataflowDetail (Prelude.Maybe Destination)
dataflowDetail_destination = Lens.lens (\DataflowDetail' {destination} -> destination) (\s@DataflowDetail' {} a -> s {destination = a} :: DataflowDetail)

-- | Error message for a dataflow.
dataflowDetail_errorMessage :: Lens.Lens' DataflowDetail (Prelude.Maybe Prelude.Text)
dataflowDetail_errorMessage = Lens.lens (\DataflowDetail' {errorMessage} -> errorMessage) (\s@DataflowDetail' {} a -> s {errorMessage = a} :: DataflowDetail)

-- | Undocumented member.
dataflowDetail_source :: Lens.Lens' DataflowDetail (Prelude.Maybe Source)
dataflowDetail_source = Lens.lens (\DataflowDetail' {source} -> source) (\s@DataflowDetail' {} a -> s {source = a} :: DataflowDetail)

instance Data.FromJSON DataflowDetail where
  parseJSON =
    Data.withObject
      "DataflowDetail"
      ( \x ->
          DataflowDetail'
            Prelude.<$> (x Data..:? "destination")
            Prelude.<*> (x Data..:? "errorMessage")
            Prelude.<*> (x Data..:? "source")
      )

instance Prelude.Hashable DataflowDetail where
  hashWithSalt _salt DataflowDetail' {..} =
    _salt
      `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` source

instance Prelude.NFData DataflowDetail where
  rnf DataflowDetail' {..} =
    Prelude.rnf destination `Prelude.seq`
      Prelude.rnf errorMessage `Prelude.seq`
        Prelude.rnf source
