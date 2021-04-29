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
-- Module      : Network.AWS.EC2.Types.CancelSpotFleetRequestsError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CancelSpotFleetRequestsError where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CancelBatchErrorCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a Spot Fleet error.
--
-- /See:/ 'newCancelSpotFleetRequestsError' smart constructor.
data CancelSpotFleetRequestsError = CancelSpotFleetRequestsError'
  { -- | The description for the error code.
    message :: Prelude.Maybe Prelude.Text,
    -- | The error code.
    code :: Prelude.Maybe CancelBatchErrorCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CancelSpotFleetRequestsError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'cancelSpotFleetRequestsError_message' - The description for the error code.
--
-- 'code', 'cancelSpotFleetRequestsError_code' - The error code.
newCancelSpotFleetRequestsError ::
  CancelSpotFleetRequestsError
newCancelSpotFleetRequestsError =
  CancelSpotFleetRequestsError'
    { message =
        Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | The description for the error code.
cancelSpotFleetRequestsError_message :: Lens.Lens' CancelSpotFleetRequestsError (Prelude.Maybe Prelude.Text)
cancelSpotFleetRequestsError_message = Lens.lens (\CancelSpotFleetRequestsError' {message} -> message) (\s@CancelSpotFleetRequestsError' {} a -> s {message = a} :: CancelSpotFleetRequestsError)

-- | The error code.
cancelSpotFleetRequestsError_code :: Lens.Lens' CancelSpotFleetRequestsError (Prelude.Maybe CancelBatchErrorCode)
cancelSpotFleetRequestsError_code = Lens.lens (\CancelSpotFleetRequestsError' {code} -> code) (\s@CancelSpotFleetRequestsError' {} a -> s {code = a} :: CancelSpotFleetRequestsError)

instance Prelude.FromXML CancelSpotFleetRequestsError where
  parseXML x =
    CancelSpotFleetRequestsError'
      Prelude.<$> (x Prelude..@? "message")
      Prelude.<*> (x Prelude..@? "code")

instance
  Prelude.Hashable
    CancelSpotFleetRequestsError

instance Prelude.NFData CancelSpotFleetRequestsError
