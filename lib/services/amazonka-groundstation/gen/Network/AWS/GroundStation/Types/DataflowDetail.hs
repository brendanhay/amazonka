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
-- Module      : Network.AWS.GroundStation.Types.DataflowDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GroundStation.Types.DataflowDetail where

import qualified Network.AWS.Core as Core
import Network.AWS.GroundStation.Types.Destination
import Network.AWS.GroundStation.Types.Source
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a dataflow edge used in a contact.
--
-- /See:/ 'newDataflowDetail' smart constructor.
data DataflowDetail = DataflowDetail'
  { destination :: Prelude.Maybe Destination,
    source :: Prelude.Maybe Source,
    -- | Error message for a dataflow.
    errorMessage :: Prelude.Maybe Prelude.Text
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
-- 'source', 'dataflowDetail_source' - Undocumented member.
--
-- 'errorMessage', 'dataflowDetail_errorMessage' - Error message for a dataflow.
newDataflowDetail ::
  DataflowDetail
newDataflowDetail =
  DataflowDetail'
    { destination = Prelude.Nothing,
      source = Prelude.Nothing,
      errorMessage = Prelude.Nothing
    }

-- | Undocumented member.
dataflowDetail_destination :: Lens.Lens' DataflowDetail (Prelude.Maybe Destination)
dataflowDetail_destination = Lens.lens (\DataflowDetail' {destination} -> destination) (\s@DataflowDetail' {} a -> s {destination = a} :: DataflowDetail)

-- | Undocumented member.
dataflowDetail_source :: Lens.Lens' DataflowDetail (Prelude.Maybe Source)
dataflowDetail_source = Lens.lens (\DataflowDetail' {source} -> source) (\s@DataflowDetail' {} a -> s {source = a} :: DataflowDetail)

-- | Error message for a dataflow.
dataflowDetail_errorMessage :: Lens.Lens' DataflowDetail (Prelude.Maybe Prelude.Text)
dataflowDetail_errorMessage = Lens.lens (\DataflowDetail' {errorMessage} -> errorMessage) (\s@DataflowDetail' {} a -> s {errorMessage = a} :: DataflowDetail)

instance Core.FromJSON DataflowDetail where
  parseJSON =
    Core.withObject
      "DataflowDetail"
      ( \x ->
          DataflowDetail'
            Prelude.<$> (x Core..:? "destination")
            Prelude.<*> (x Core..:? "source")
            Prelude.<*> (x Core..:? "errorMessage")
      )

instance Prelude.Hashable DataflowDetail

instance Prelude.NFData DataflowDetail
