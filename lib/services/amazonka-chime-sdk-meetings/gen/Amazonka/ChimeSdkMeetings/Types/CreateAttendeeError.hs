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
-- Module      : Amazonka.ChimeSdkMeetings.Types.CreateAttendeeError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMeetings.Types.CreateAttendeeError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The list of errors returned when errors are encountered during the
-- BatchCreateAttendee and CreateAttendee actions. This includes external
-- user IDs, error codes, and error messages.
--
-- /See:/ 'newCreateAttendeeError' smart constructor.
data CreateAttendeeError = CreateAttendeeError'
  { -- | The error code.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The error message.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Chime SDK external user ID. An idempotency token. Links the
    -- attendee to an identity managed by a builder application.
    externalUserId :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAttendeeError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'createAttendeeError_errorCode' - The error code.
--
-- 'errorMessage', 'createAttendeeError_errorMessage' - The error message.
--
-- 'externalUserId', 'createAttendeeError_externalUserId' - The Amazon Chime SDK external user ID. An idempotency token. Links the
-- attendee to an identity managed by a builder application.
newCreateAttendeeError ::
  CreateAttendeeError
newCreateAttendeeError =
  CreateAttendeeError'
    { errorCode = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      externalUserId = Prelude.Nothing
    }

-- | The error code.
createAttendeeError_errorCode :: Lens.Lens' CreateAttendeeError (Prelude.Maybe Prelude.Text)
createAttendeeError_errorCode = Lens.lens (\CreateAttendeeError' {errorCode} -> errorCode) (\s@CreateAttendeeError' {} a -> s {errorCode = a} :: CreateAttendeeError)

-- | The error message.
createAttendeeError_errorMessage :: Lens.Lens' CreateAttendeeError (Prelude.Maybe Prelude.Text)
createAttendeeError_errorMessage = Lens.lens (\CreateAttendeeError' {errorMessage} -> errorMessage) (\s@CreateAttendeeError' {} a -> s {errorMessage = a} :: CreateAttendeeError)

-- | The Amazon Chime SDK external user ID. An idempotency token. Links the
-- attendee to an identity managed by a builder application.
createAttendeeError_externalUserId :: Lens.Lens' CreateAttendeeError (Prelude.Maybe Prelude.Text)
createAttendeeError_externalUserId = Lens.lens (\CreateAttendeeError' {externalUserId} -> externalUserId) (\s@CreateAttendeeError' {} a -> s {externalUserId = a} :: CreateAttendeeError) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON CreateAttendeeError where
  parseJSON =
    Data.withObject
      "CreateAttendeeError"
      ( \x ->
          CreateAttendeeError'
            Prelude.<$> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "ExternalUserId")
      )

instance Prelude.Hashable CreateAttendeeError where
  hashWithSalt _salt CreateAttendeeError' {..} =
    _salt
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` externalUserId

instance Prelude.NFData CreateAttendeeError where
  rnf CreateAttendeeError' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf externalUserId
