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
-- Module      : Amazonka.EC2.Types.CancelSpotFleetRequestsErrorItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.CancelSpotFleetRequestsErrorItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.CancelSpotFleetRequestsError
import qualified Amazonka.Prelude as Prelude

-- | Describes a Spot Fleet request that was not successfully canceled.
--
-- /See:/ 'newCancelSpotFleetRequestsErrorItem' smart constructor.
data CancelSpotFleetRequestsErrorItem = CancelSpotFleetRequestsErrorItem'
  { -- | The ID of the Spot Fleet request.
    spotFleetRequestId :: Prelude.Maybe Prelude.Text,
    -- | The error.
    error :: Prelude.Maybe CancelSpotFleetRequestsError
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelSpotFleetRequestsErrorItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'spotFleetRequestId', 'cancelSpotFleetRequestsErrorItem_spotFleetRequestId' - The ID of the Spot Fleet request.
--
-- 'error', 'cancelSpotFleetRequestsErrorItem_error' - The error.
newCancelSpotFleetRequestsErrorItem ::
  CancelSpotFleetRequestsErrorItem
newCancelSpotFleetRequestsErrorItem =
  CancelSpotFleetRequestsErrorItem'
    { spotFleetRequestId =
        Prelude.Nothing,
      error = Prelude.Nothing
    }

-- | The ID of the Spot Fleet request.
cancelSpotFleetRequestsErrorItem_spotFleetRequestId :: Lens.Lens' CancelSpotFleetRequestsErrorItem (Prelude.Maybe Prelude.Text)
cancelSpotFleetRequestsErrorItem_spotFleetRequestId = Lens.lens (\CancelSpotFleetRequestsErrorItem' {spotFleetRequestId} -> spotFleetRequestId) (\s@CancelSpotFleetRequestsErrorItem' {} a -> s {spotFleetRequestId = a} :: CancelSpotFleetRequestsErrorItem)

-- | The error.
cancelSpotFleetRequestsErrorItem_error :: Lens.Lens' CancelSpotFleetRequestsErrorItem (Prelude.Maybe CancelSpotFleetRequestsError)
cancelSpotFleetRequestsErrorItem_error = Lens.lens (\CancelSpotFleetRequestsErrorItem' {error} -> error) (\s@CancelSpotFleetRequestsErrorItem' {} a -> s {error = a} :: CancelSpotFleetRequestsErrorItem)

instance
  Data.FromXML
    CancelSpotFleetRequestsErrorItem
  where
  parseXML x =
    CancelSpotFleetRequestsErrorItem'
      Prelude.<$> (x Data..@? "spotFleetRequestId")
      Prelude.<*> (x Data..@? "error")

instance
  Prelude.Hashable
    CancelSpotFleetRequestsErrorItem
  where
  hashWithSalt
    _salt
    CancelSpotFleetRequestsErrorItem' {..} =
      _salt `Prelude.hashWithSalt` spotFleetRequestId
        `Prelude.hashWithSalt` error

instance
  Prelude.NFData
    CancelSpotFleetRequestsErrorItem
  where
  rnf CancelSpotFleetRequestsErrorItem' {..} =
    Prelude.rnf spotFleetRequestId
      `Prelude.seq` Prelude.rnf error
