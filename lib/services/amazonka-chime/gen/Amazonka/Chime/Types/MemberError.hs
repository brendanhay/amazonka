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
-- Module      : Amazonka.Chime.Types.MemberError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.MemberError where

import Amazonka.Chime.Types.ErrorCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The list of errors returned when a member action results in an error.
--
-- /See:/ 'newMemberError' smart constructor.
data MemberError = MemberError'
  { -- | The error code.
    errorCode :: Prelude.Maybe ErrorCode,
    -- | The error message.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The member ID.
    memberId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MemberError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'memberError_errorCode' - The error code.
--
-- 'errorMessage', 'memberError_errorMessage' - The error message.
--
-- 'memberId', 'memberError_memberId' - The member ID.
newMemberError ::
  MemberError
newMemberError =
  MemberError'
    { errorCode = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      memberId = Prelude.Nothing
    }

-- | The error code.
memberError_errorCode :: Lens.Lens' MemberError (Prelude.Maybe ErrorCode)
memberError_errorCode = Lens.lens (\MemberError' {errorCode} -> errorCode) (\s@MemberError' {} a -> s {errorCode = a} :: MemberError)

-- | The error message.
memberError_errorMessage :: Lens.Lens' MemberError (Prelude.Maybe Prelude.Text)
memberError_errorMessage = Lens.lens (\MemberError' {errorMessage} -> errorMessage) (\s@MemberError' {} a -> s {errorMessage = a} :: MemberError)

-- | The member ID.
memberError_memberId :: Lens.Lens' MemberError (Prelude.Maybe Prelude.Text)
memberError_memberId = Lens.lens (\MemberError' {memberId} -> memberId) (\s@MemberError' {} a -> s {memberId = a} :: MemberError)

instance Data.FromJSON MemberError where
  parseJSON =
    Data.withObject
      "MemberError"
      ( \x ->
          MemberError'
            Prelude.<$> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "MemberId")
      )

instance Prelude.Hashable MemberError where
  hashWithSalt _salt MemberError' {..} =
    _salt `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` memberId

instance Prelude.NFData MemberError where
  rnf MemberError' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf memberId
