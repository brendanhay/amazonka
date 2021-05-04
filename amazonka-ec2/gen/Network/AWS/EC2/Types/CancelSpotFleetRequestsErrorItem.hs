{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.Types.CancelSpotFleetRequestsErrorItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CancelSpotFleetRequestsErrorItem where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CancelSpotFleetRequestsError
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a Spot Fleet request that was not successfully canceled.
--
-- /See:/ 'newCancelSpotFleetRequestsErrorItem' smart constructor.
data CancelSpotFleetRequestsErrorItem = CancelSpotFleetRequestsErrorItem'
  { -- | The error.
    error :: Prelude.Maybe CancelSpotFleetRequestsError,
    -- | The ID of the Spot Fleet request.
    spotFleetRequestId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CancelSpotFleetRequestsErrorItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'cancelSpotFleetRequestsErrorItem_error' - The error.
--
-- 'spotFleetRequestId', 'cancelSpotFleetRequestsErrorItem_spotFleetRequestId' - The ID of the Spot Fleet request.
newCancelSpotFleetRequestsErrorItem ::
  CancelSpotFleetRequestsErrorItem
newCancelSpotFleetRequestsErrorItem =
  CancelSpotFleetRequestsErrorItem'
    { error =
        Prelude.Nothing,
      spotFleetRequestId = Prelude.Nothing
    }

-- | The error.
cancelSpotFleetRequestsErrorItem_error :: Lens.Lens' CancelSpotFleetRequestsErrorItem (Prelude.Maybe CancelSpotFleetRequestsError)
cancelSpotFleetRequestsErrorItem_error = Lens.lens (\CancelSpotFleetRequestsErrorItem' {error} -> error) (\s@CancelSpotFleetRequestsErrorItem' {} a -> s {error = a} :: CancelSpotFleetRequestsErrorItem)

-- | The ID of the Spot Fleet request.
cancelSpotFleetRequestsErrorItem_spotFleetRequestId :: Lens.Lens' CancelSpotFleetRequestsErrorItem (Prelude.Maybe Prelude.Text)
cancelSpotFleetRequestsErrorItem_spotFleetRequestId = Lens.lens (\CancelSpotFleetRequestsErrorItem' {spotFleetRequestId} -> spotFleetRequestId) (\s@CancelSpotFleetRequestsErrorItem' {} a -> s {spotFleetRequestId = a} :: CancelSpotFleetRequestsErrorItem)

instance
  Prelude.FromXML
    CancelSpotFleetRequestsErrorItem
  where
  parseXML x =
    CancelSpotFleetRequestsErrorItem'
      Prelude.<$> (x Prelude..@? "error")
      Prelude.<*> (x Prelude..@? "spotFleetRequestId")

instance
  Prelude.Hashable
    CancelSpotFleetRequestsErrorItem

instance
  Prelude.NFData
    CancelSpotFleetRequestsErrorItem
