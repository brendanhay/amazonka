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
-- Module      : Amazonka.EC2.Types.CancelledSpotInstanceRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.CancelledSpotInstanceRequest where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.CancelSpotInstanceRequestState
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a request to cancel a Spot Instance.
--
-- /See:/ 'newCancelledSpotInstanceRequest' smart constructor.
data CancelledSpotInstanceRequest = CancelledSpotInstanceRequest'
  { -- | The state of the Spot Instance request.
    state :: Prelude.Maybe CancelSpotInstanceRequestState,
    -- | The ID of the Spot Instance request.
    spotInstanceRequestId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelledSpotInstanceRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'cancelledSpotInstanceRequest_state' - The state of the Spot Instance request.
--
-- 'spotInstanceRequestId', 'cancelledSpotInstanceRequest_spotInstanceRequestId' - The ID of the Spot Instance request.
newCancelledSpotInstanceRequest ::
  CancelledSpotInstanceRequest
newCancelledSpotInstanceRequest =
  CancelledSpotInstanceRequest'
    { state =
        Prelude.Nothing,
      spotInstanceRequestId = Prelude.Nothing
    }

-- | The state of the Spot Instance request.
cancelledSpotInstanceRequest_state :: Lens.Lens' CancelledSpotInstanceRequest (Prelude.Maybe CancelSpotInstanceRequestState)
cancelledSpotInstanceRequest_state = Lens.lens (\CancelledSpotInstanceRequest' {state} -> state) (\s@CancelledSpotInstanceRequest' {} a -> s {state = a} :: CancelledSpotInstanceRequest)

-- | The ID of the Spot Instance request.
cancelledSpotInstanceRequest_spotInstanceRequestId :: Lens.Lens' CancelledSpotInstanceRequest (Prelude.Maybe Prelude.Text)
cancelledSpotInstanceRequest_spotInstanceRequestId = Lens.lens (\CancelledSpotInstanceRequest' {spotInstanceRequestId} -> spotInstanceRequestId) (\s@CancelledSpotInstanceRequest' {} a -> s {spotInstanceRequestId = a} :: CancelledSpotInstanceRequest)

instance Core.FromXML CancelledSpotInstanceRequest where
  parseXML x =
    CancelledSpotInstanceRequest'
      Prelude.<$> (x Core..@? "state")
      Prelude.<*> (x Core..@? "spotInstanceRequestId")

instance
  Prelude.Hashable
    CancelledSpotInstanceRequest
  where
  hashWithSalt _salt CancelledSpotInstanceRequest' {..} =
    _salt `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` spotInstanceRequestId

instance Prelude.NFData CancelledSpotInstanceRequest where
  rnf CancelledSpotInstanceRequest' {..} =
    Prelude.rnf state
      `Prelude.seq` Prelude.rnf spotInstanceRequestId
