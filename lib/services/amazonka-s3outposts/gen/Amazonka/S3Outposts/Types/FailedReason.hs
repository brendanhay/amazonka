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
-- Module      : Amazonka.S3Outposts.Types.FailedReason
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3Outposts.Types.FailedReason where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The failure reason, if any, for a create or delete endpoint operation.
--
-- /See:/ 'newFailedReason' smart constructor.
data FailedReason = FailedReason'
  { -- | The failure code, if any, for a create or delete endpoint operation.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | Additional error details describing the endpoint failure and recommended
    -- action.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailedReason' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'failedReason_errorCode' - The failure code, if any, for a create or delete endpoint operation.
--
-- 'message', 'failedReason_message' - Additional error details describing the endpoint failure and recommended
-- action.
newFailedReason ::
  FailedReason
newFailedReason =
  FailedReason'
    { errorCode = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The failure code, if any, for a create or delete endpoint operation.
failedReason_errorCode :: Lens.Lens' FailedReason (Prelude.Maybe Prelude.Text)
failedReason_errorCode = Lens.lens (\FailedReason' {errorCode} -> errorCode) (\s@FailedReason' {} a -> s {errorCode = a} :: FailedReason)

-- | Additional error details describing the endpoint failure and recommended
-- action.
failedReason_message :: Lens.Lens' FailedReason (Prelude.Maybe Prelude.Text)
failedReason_message = Lens.lens (\FailedReason' {message} -> message) (\s@FailedReason' {} a -> s {message = a} :: FailedReason)

instance Data.FromJSON FailedReason where
  parseJSON =
    Data.withObject
      "FailedReason"
      ( \x ->
          FailedReason'
            Prelude.<$> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "Message")
      )

instance Prelude.Hashable FailedReason where
  hashWithSalt _salt FailedReason' {..} =
    _salt
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` message

instance Prelude.NFData FailedReason where
  rnf FailedReason' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf message
