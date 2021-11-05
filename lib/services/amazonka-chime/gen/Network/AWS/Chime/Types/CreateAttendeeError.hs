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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.CreateAttendeeError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
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
    externalUserId :: Prelude.Maybe (Core.Sensitive Prelude.Text)
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
createAttendeeError_externalUserId = Lens.lens (\CreateAttendeeError' {externalUserId} -> externalUserId) (\s@CreateAttendeeError' {} a -> s {externalUserId = a} :: CreateAttendeeError) Prelude.. Lens.mapping Core._Sensitive

instance Core.FromJSON CreateAttendeeError where
  parseJSON =
    Core.withObject
      "CreateAttendeeError"
      ( \x ->
          CreateAttendeeError'
            Prelude.<$> (x Core..:? "ErrorCode")
            Prelude.<*> (x Core..:? "ErrorMessage")
            Prelude.<*> (x Core..:? "ExternalUserId")
      )

instance Prelude.Hashable CreateAttendeeError

instance Prelude.NFData CreateAttendeeError
