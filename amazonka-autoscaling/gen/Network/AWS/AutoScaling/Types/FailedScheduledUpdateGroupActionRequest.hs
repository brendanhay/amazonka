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
-- Module      : Network.AWS.AutoScaling.Types.FailedScheduledUpdateGroupActionRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.FailedScheduledUpdateGroupActionRequest where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a scheduled action that could not be created, updated, or
-- deleted.
--
-- /See:/ 'newFailedScheduledUpdateGroupActionRequest' smart constructor.
data FailedScheduledUpdateGroupActionRequest = FailedScheduledUpdateGroupActionRequest'
  { -- | The error message accompanying the error code.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The error code.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The name of the scheduled action.
    scheduledActionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FailedScheduledUpdateGroupActionRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorMessage', 'failedScheduledUpdateGroupActionRequest_errorMessage' - The error message accompanying the error code.
--
-- 'errorCode', 'failedScheduledUpdateGroupActionRequest_errorCode' - The error code.
--
-- 'scheduledActionName', 'failedScheduledUpdateGroupActionRequest_scheduledActionName' - The name of the scheduled action.
newFailedScheduledUpdateGroupActionRequest ::
  -- | 'scheduledActionName'
  Prelude.Text ->
  FailedScheduledUpdateGroupActionRequest
newFailedScheduledUpdateGroupActionRequest
  pScheduledActionName_ =
    FailedScheduledUpdateGroupActionRequest'
      { errorMessage =
          Prelude.Nothing,
        errorCode = Prelude.Nothing,
        scheduledActionName =
          pScheduledActionName_
      }

-- | The error message accompanying the error code.
failedScheduledUpdateGroupActionRequest_errorMessage :: Lens.Lens' FailedScheduledUpdateGroupActionRequest (Prelude.Maybe Prelude.Text)
failedScheduledUpdateGroupActionRequest_errorMessage = Lens.lens (\FailedScheduledUpdateGroupActionRequest' {errorMessage} -> errorMessage) (\s@FailedScheduledUpdateGroupActionRequest' {} a -> s {errorMessage = a} :: FailedScheduledUpdateGroupActionRequest)

-- | The error code.
failedScheduledUpdateGroupActionRequest_errorCode :: Lens.Lens' FailedScheduledUpdateGroupActionRequest (Prelude.Maybe Prelude.Text)
failedScheduledUpdateGroupActionRequest_errorCode = Lens.lens (\FailedScheduledUpdateGroupActionRequest' {errorCode} -> errorCode) (\s@FailedScheduledUpdateGroupActionRequest' {} a -> s {errorCode = a} :: FailedScheduledUpdateGroupActionRequest)

-- | The name of the scheduled action.
failedScheduledUpdateGroupActionRequest_scheduledActionName :: Lens.Lens' FailedScheduledUpdateGroupActionRequest Prelude.Text
failedScheduledUpdateGroupActionRequest_scheduledActionName = Lens.lens (\FailedScheduledUpdateGroupActionRequest' {scheduledActionName} -> scheduledActionName) (\s@FailedScheduledUpdateGroupActionRequest' {} a -> s {scheduledActionName = a} :: FailedScheduledUpdateGroupActionRequest)

instance
  Prelude.FromXML
    FailedScheduledUpdateGroupActionRequest
  where
  parseXML x =
    FailedScheduledUpdateGroupActionRequest'
      Prelude.<$> (x Prelude..@? "ErrorMessage")
        Prelude.<*> (x Prelude..@? "ErrorCode")
        Prelude.<*> (x Prelude..@ "ScheduledActionName")

instance
  Prelude.Hashable
    FailedScheduledUpdateGroupActionRequest

instance
  Prelude.NFData
    FailedScheduledUpdateGroupActionRequest
