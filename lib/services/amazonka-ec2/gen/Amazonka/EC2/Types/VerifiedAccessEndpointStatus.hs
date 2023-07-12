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
-- Module      : Amazonka.EC2.Types.VerifiedAccessEndpointStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VerifiedAccessEndpointStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.VerifiedAccessEndpointStatusCode
import qualified Amazonka.Prelude as Prelude

-- | Describes the status of a Verified Access endpoint.
--
-- /See:/ 'newVerifiedAccessEndpointStatus' smart constructor.
data VerifiedAccessEndpointStatus = VerifiedAccessEndpointStatus'
  { -- | The status code of the Verified Access endpoint.
    code :: Prelude.Maybe VerifiedAccessEndpointStatusCode,
    -- | The status message of the Verified Access endpoint.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifiedAccessEndpointStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'verifiedAccessEndpointStatus_code' - The status code of the Verified Access endpoint.
--
-- 'message', 'verifiedAccessEndpointStatus_message' - The status message of the Verified Access endpoint.
newVerifiedAccessEndpointStatus ::
  VerifiedAccessEndpointStatus
newVerifiedAccessEndpointStatus =
  VerifiedAccessEndpointStatus'
    { code =
        Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The status code of the Verified Access endpoint.
verifiedAccessEndpointStatus_code :: Lens.Lens' VerifiedAccessEndpointStatus (Prelude.Maybe VerifiedAccessEndpointStatusCode)
verifiedAccessEndpointStatus_code = Lens.lens (\VerifiedAccessEndpointStatus' {code} -> code) (\s@VerifiedAccessEndpointStatus' {} a -> s {code = a} :: VerifiedAccessEndpointStatus)

-- | The status message of the Verified Access endpoint.
verifiedAccessEndpointStatus_message :: Lens.Lens' VerifiedAccessEndpointStatus (Prelude.Maybe Prelude.Text)
verifiedAccessEndpointStatus_message = Lens.lens (\VerifiedAccessEndpointStatus' {message} -> message) (\s@VerifiedAccessEndpointStatus' {} a -> s {message = a} :: VerifiedAccessEndpointStatus)

instance Data.FromXML VerifiedAccessEndpointStatus where
  parseXML x =
    VerifiedAccessEndpointStatus'
      Prelude.<$> (x Data..@? "code")
      Prelude.<*> (x Data..@? "message")

instance
  Prelude.Hashable
    VerifiedAccessEndpointStatus
  where
  hashWithSalt _salt VerifiedAccessEndpointStatus' {..} =
    _salt
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message

instance Prelude.NFData VerifiedAccessEndpointStatus where
  rnf VerifiedAccessEndpointStatus' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
