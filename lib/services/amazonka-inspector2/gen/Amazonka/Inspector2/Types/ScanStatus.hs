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
-- Module      : Amazonka.Inspector2.Types.ScanStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.ScanStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.ScanStatusCode
import Amazonka.Inspector2.Types.ScanStatusReason
import qualified Amazonka.Prelude as Prelude

-- | The status of the scan.
--
-- /See:/ 'newScanStatus' smart constructor.
data ScanStatus = ScanStatus'
  { -- | The reason for the scan.
    reason :: ScanStatusReason,
    -- | The status code of the scan.
    statusCode :: ScanStatusCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScanStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reason', 'scanStatus_reason' - The reason for the scan.
--
-- 'statusCode', 'scanStatus_statusCode' - The status code of the scan.
newScanStatus ::
  -- | 'reason'
  ScanStatusReason ->
  -- | 'statusCode'
  ScanStatusCode ->
  ScanStatus
newScanStatus pReason_ pStatusCode_ =
  ScanStatus'
    { reason = pReason_,
      statusCode = pStatusCode_
    }

-- | The reason for the scan.
scanStatus_reason :: Lens.Lens' ScanStatus ScanStatusReason
scanStatus_reason = Lens.lens (\ScanStatus' {reason} -> reason) (\s@ScanStatus' {} a -> s {reason = a} :: ScanStatus)

-- | The status code of the scan.
scanStatus_statusCode :: Lens.Lens' ScanStatus ScanStatusCode
scanStatus_statusCode = Lens.lens (\ScanStatus' {statusCode} -> statusCode) (\s@ScanStatus' {} a -> s {statusCode = a} :: ScanStatus)

instance Data.FromJSON ScanStatus where
  parseJSON =
    Data.withObject
      "ScanStatus"
      ( \x ->
          ScanStatus'
            Prelude.<$> (x Data..: "reason")
            Prelude.<*> (x Data..: "statusCode")
      )

instance Prelude.Hashable ScanStatus where
  hashWithSalt _salt ScanStatus' {..} =
    _salt `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` statusCode

instance Prelude.NFData ScanStatus where
  rnf ScanStatus' {..} =
    Prelude.rnf reason
      `Prelude.seq` Prelude.rnf statusCode
