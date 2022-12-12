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
-- Module      : Amazonka.EC2.Types.VerifiedAccessLogDeliveryStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VerifiedAccessLogDeliveryStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.VerifiedAccessLogDeliveryStatusCode
import qualified Amazonka.Prelude as Prelude

-- | Describes a log delivery status.
--
-- /See:/ 'newVerifiedAccessLogDeliveryStatus' smart constructor.
data VerifiedAccessLogDeliveryStatus = VerifiedAccessLogDeliveryStatus'
  { -- | The status code.
    code :: Prelude.Maybe VerifiedAccessLogDeliveryStatusCode,
    -- | The status message.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifiedAccessLogDeliveryStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'verifiedAccessLogDeliveryStatus_code' - The status code.
--
-- 'message', 'verifiedAccessLogDeliveryStatus_message' - The status message.
newVerifiedAccessLogDeliveryStatus ::
  VerifiedAccessLogDeliveryStatus
newVerifiedAccessLogDeliveryStatus =
  VerifiedAccessLogDeliveryStatus'
    { code =
        Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The status code.
verifiedAccessLogDeliveryStatus_code :: Lens.Lens' VerifiedAccessLogDeliveryStatus (Prelude.Maybe VerifiedAccessLogDeliveryStatusCode)
verifiedAccessLogDeliveryStatus_code = Lens.lens (\VerifiedAccessLogDeliveryStatus' {code} -> code) (\s@VerifiedAccessLogDeliveryStatus' {} a -> s {code = a} :: VerifiedAccessLogDeliveryStatus)

-- | The status message.
verifiedAccessLogDeliveryStatus_message :: Lens.Lens' VerifiedAccessLogDeliveryStatus (Prelude.Maybe Prelude.Text)
verifiedAccessLogDeliveryStatus_message = Lens.lens (\VerifiedAccessLogDeliveryStatus' {message} -> message) (\s@VerifiedAccessLogDeliveryStatus' {} a -> s {message = a} :: VerifiedAccessLogDeliveryStatus)

instance Data.FromXML VerifiedAccessLogDeliveryStatus where
  parseXML x =
    VerifiedAccessLogDeliveryStatus'
      Prelude.<$> (x Data..@? "code")
      Prelude.<*> (x Data..@? "message")

instance
  Prelude.Hashable
    VerifiedAccessLogDeliveryStatus
  where
  hashWithSalt
    _salt
    VerifiedAccessLogDeliveryStatus' {..} =
      _salt `Prelude.hashWithSalt` code
        `Prelude.hashWithSalt` message

instance
  Prelude.NFData
    VerifiedAccessLogDeliveryStatus
  where
  rnf VerifiedAccessLogDeliveryStatus' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
