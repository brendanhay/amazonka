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
-- Module      : Amazonka.EC2.Types.CancelSpotFleetRequestsError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.CancelSpotFleetRequestsError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.CancelBatchErrorCode
import qualified Amazonka.Prelude as Prelude

-- | Describes a Spot Fleet error.
--
-- /See:/ 'newCancelSpotFleetRequestsError' smart constructor.
data CancelSpotFleetRequestsError = CancelSpotFleetRequestsError'
  { -- | The error code.
    code :: Prelude.Maybe CancelBatchErrorCode,
    -- | The description for the error code.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelSpotFleetRequestsError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'cancelSpotFleetRequestsError_code' - The error code.
--
-- 'message', 'cancelSpotFleetRequestsError_message' - The description for the error code.
newCancelSpotFleetRequestsError ::
  CancelSpotFleetRequestsError
newCancelSpotFleetRequestsError =
  CancelSpotFleetRequestsError'
    { code =
        Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The error code.
cancelSpotFleetRequestsError_code :: Lens.Lens' CancelSpotFleetRequestsError (Prelude.Maybe CancelBatchErrorCode)
cancelSpotFleetRequestsError_code = Lens.lens (\CancelSpotFleetRequestsError' {code} -> code) (\s@CancelSpotFleetRequestsError' {} a -> s {code = a} :: CancelSpotFleetRequestsError)

-- | The description for the error code.
cancelSpotFleetRequestsError_message :: Lens.Lens' CancelSpotFleetRequestsError (Prelude.Maybe Prelude.Text)
cancelSpotFleetRequestsError_message = Lens.lens (\CancelSpotFleetRequestsError' {message} -> message) (\s@CancelSpotFleetRequestsError' {} a -> s {message = a} :: CancelSpotFleetRequestsError)

instance Data.FromXML CancelSpotFleetRequestsError where
  parseXML x =
    CancelSpotFleetRequestsError'
      Prelude.<$> (x Data..@? "code")
      Prelude.<*> (x Data..@? "message")

instance
  Prelude.Hashable
    CancelSpotFleetRequestsError
  where
  hashWithSalt _salt CancelSpotFleetRequestsError' {..} =
    _salt
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message

instance Prelude.NFData CancelSpotFleetRequestsError where
  rnf CancelSpotFleetRequestsError' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
