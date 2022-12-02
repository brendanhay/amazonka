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
-- Module      : Amazonka.Chime.Types.CreateAttendeeError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.CreateAttendeeError where

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
  { -- | The Amazon Chime SDK external user ID. An idempotency token. Links the
    -- attendee to an identity managed by a builder application.
    externalUserId :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The error message.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The error code.
    errorCode :: Prelude.Maybe Prelude.Text
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
-- 'externalUserId', 'createAttendeeError_externalUserId' - The Amazon Chime SDK external user ID. An idempotency token. Links the
-- attendee to an identity managed by a builder application.
--
-- 'errorMessage', 'createAttendeeError_errorMessage' - The error message.
--
-- 'errorCode', 'createAttendeeError_errorCode' - The error code.
newCreateAttendeeError ::
  CreateAttendeeError
newCreateAttendeeError =
  CreateAttendeeError'
    { externalUserId =
        Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | The Amazon Chime SDK external user ID. An idempotency token. Links the
-- attendee to an identity managed by a builder application.
createAttendeeError_externalUserId :: Lens.Lens' CreateAttendeeError (Prelude.Maybe Prelude.Text)
createAttendeeError_externalUserId = Lens.lens (\CreateAttendeeError' {externalUserId} -> externalUserId) (\s@CreateAttendeeError' {} a -> s {externalUserId = a} :: CreateAttendeeError) Prelude.. Lens.mapping Data._Sensitive

-- | The error message.
createAttendeeError_errorMessage :: Lens.Lens' CreateAttendeeError (Prelude.Maybe Prelude.Text)
createAttendeeError_errorMessage = Lens.lens (\CreateAttendeeError' {errorMessage} -> errorMessage) (\s@CreateAttendeeError' {} a -> s {errorMessage = a} :: CreateAttendeeError)

-- | The error code.
createAttendeeError_errorCode :: Lens.Lens' CreateAttendeeError (Prelude.Maybe Prelude.Text)
createAttendeeError_errorCode = Lens.lens (\CreateAttendeeError' {errorCode} -> errorCode) (\s@CreateAttendeeError' {} a -> s {errorCode = a} :: CreateAttendeeError)

instance Data.FromJSON CreateAttendeeError where
  parseJSON =
    Data.withObject
      "CreateAttendeeError"
      ( \x ->
          CreateAttendeeError'
            Prelude.<$> (x Data..:? "ExternalUserId")
            Prelude.<*> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "ErrorCode")
      )

instance Prelude.Hashable CreateAttendeeError where
  hashWithSalt _salt CreateAttendeeError' {..} =
    _salt `Prelude.hashWithSalt` externalUserId
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` errorCode

instance Prelude.NFData CreateAttendeeError where
  rnf CreateAttendeeError' {..} =
    Prelude.rnf externalUserId
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf errorCode
