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
-- Module      : Amazonka.AutoScaling.Types.FailedScheduledUpdateGroupActionRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.FailedScheduledUpdateGroupActionRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a scheduled action that could not be created, updated, or
-- deleted.
--
-- /See:/ 'newFailedScheduledUpdateGroupActionRequest' smart constructor.
data FailedScheduledUpdateGroupActionRequest = FailedScheduledUpdateGroupActionRequest'
  { -- | The error code.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The error message accompanying the error code.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The name of the scheduled action.
    scheduledActionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailedScheduledUpdateGroupActionRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'failedScheduledUpdateGroupActionRequest_errorCode' - The error code.
--
-- 'errorMessage', 'failedScheduledUpdateGroupActionRequest_errorMessage' - The error message accompanying the error code.
--
-- 'scheduledActionName', 'failedScheduledUpdateGroupActionRequest_scheduledActionName' - The name of the scheduled action.
newFailedScheduledUpdateGroupActionRequest ::
  -- | 'scheduledActionName'
  Prelude.Text ->
  FailedScheduledUpdateGroupActionRequest
newFailedScheduledUpdateGroupActionRequest
  pScheduledActionName_ =
    FailedScheduledUpdateGroupActionRequest'
      { errorCode =
          Prelude.Nothing,
        errorMessage = Prelude.Nothing,
        scheduledActionName =
          pScheduledActionName_
      }

-- | The error code.
failedScheduledUpdateGroupActionRequest_errorCode :: Lens.Lens' FailedScheduledUpdateGroupActionRequest (Prelude.Maybe Prelude.Text)
failedScheduledUpdateGroupActionRequest_errorCode = Lens.lens (\FailedScheduledUpdateGroupActionRequest' {errorCode} -> errorCode) (\s@FailedScheduledUpdateGroupActionRequest' {} a -> s {errorCode = a} :: FailedScheduledUpdateGroupActionRequest)

-- | The error message accompanying the error code.
failedScheduledUpdateGroupActionRequest_errorMessage :: Lens.Lens' FailedScheduledUpdateGroupActionRequest (Prelude.Maybe Prelude.Text)
failedScheduledUpdateGroupActionRequest_errorMessage = Lens.lens (\FailedScheduledUpdateGroupActionRequest' {errorMessage} -> errorMessage) (\s@FailedScheduledUpdateGroupActionRequest' {} a -> s {errorMessage = a} :: FailedScheduledUpdateGroupActionRequest)

-- | The name of the scheduled action.
failedScheduledUpdateGroupActionRequest_scheduledActionName :: Lens.Lens' FailedScheduledUpdateGroupActionRequest Prelude.Text
failedScheduledUpdateGroupActionRequest_scheduledActionName = Lens.lens (\FailedScheduledUpdateGroupActionRequest' {scheduledActionName} -> scheduledActionName) (\s@FailedScheduledUpdateGroupActionRequest' {} a -> s {scheduledActionName = a} :: FailedScheduledUpdateGroupActionRequest)

instance
  Data.FromXML
    FailedScheduledUpdateGroupActionRequest
  where
  parseXML x =
    FailedScheduledUpdateGroupActionRequest'
      Prelude.<$> (x Data..@? "ErrorCode")
      Prelude.<*> (x Data..@? "ErrorMessage")
      Prelude.<*> (x Data..@ "ScheduledActionName")

instance
  Prelude.Hashable
    FailedScheduledUpdateGroupActionRequest
  where
  hashWithSalt
    _salt
    FailedScheduledUpdateGroupActionRequest' {..} =
      _salt
        `Prelude.hashWithSalt` errorCode
        `Prelude.hashWithSalt` errorMessage
        `Prelude.hashWithSalt` scheduledActionName

instance
  Prelude.NFData
    FailedScheduledUpdateGroupActionRequest
  where
  rnf FailedScheduledUpdateGroupActionRequest' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf scheduledActionName
