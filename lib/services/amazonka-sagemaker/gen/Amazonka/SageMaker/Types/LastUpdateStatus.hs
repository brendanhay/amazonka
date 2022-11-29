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
-- Module      : Amazonka.SageMaker.Types.LastUpdateStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.LastUpdateStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.LastUpdateStatusValue

-- | A value that indicates whether the update was successful.
--
-- /See:/ 'newLastUpdateStatus' smart constructor.
data LastUpdateStatus = LastUpdateStatus'
  { -- | If the update wasn\'t successful, indicates the reason why it failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the update was made successful.
    status :: LastUpdateStatusValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LastUpdateStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureReason', 'lastUpdateStatus_failureReason' - If the update wasn\'t successful, indicates the reason why it failed.
--
-- 'status', 'lastUpdateStatus_status' - A value that indicates whether the update was made successful.
newLastUpdateStatus ::
  -- | 'status'
  LastUpdateStatusValue ->
  LastUpdateStatus
newLastUpdateStatus pStatus_ =
  LastUpdateStatus'
    { failureReason = Prelude.Nothing,
      status = pStatus_
    }

-- | If the update wasn\'t successful, indicates the reason why it failed.
lastUpdateStatus_failureReason :: Lens.Lens' LastUpdateStatus (Prelude.Maybe Prelude.Text)
lastUpdateStatus_failureReason = Lens.lens (\LastUpdateStatus' {failureReason} -> failureReason) (\s@LastUpdateStatus' {} a -> s {failureReason = a} :: LastUpdateStatus)

-- | A value that indicates whether the update was made successful.
lastUpdateStatus_status :: Lens.Lens' LastUpdateStatus LastUpdateStatusValue
lastUpdateStatus_status = Lens.lens (\LastUpdateStatus' {status} -> status) (\s@LastUpdateStatus' {} a -> s {status = a} :: LastUpdateStatus)

instance Core.FromJSON LastUpdateStatus where
  parseJSON =
    Core.withObject
      "LastUpdateStatus"
      ( \x ->
          LastUpdateStatus'
            Prelude.<$> (x Core..:? "FailureReason")
            Prelude.<*> (x Core..: "Status")
      )

instance Prelude.Hashable LastUpdateStatus where
  hashWithSalt _salt LastUpdateStatus' {..} =
    _salt `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` status

instance Prelude.NFData LastUpdateStatus where
  rnf LastUpdateStatus' {..} =
    Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf status
