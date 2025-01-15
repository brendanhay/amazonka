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
-- Module      : Amazonka.CognitoIdentity.Types.UnprocessedIdentityId
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentity.Types.UnprocessedIdentityId where

import Amazonka.CognitoIdentity.Types.CognitoErrorCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An array of UnprocessedIdentityId objects, each of which contains an
-- ErrorCode and IdentityId.
--
-- /See:/ 'newUnprocessedIdentityId' smart constructor.
data UnprocessedIdentityId = UnprocessedIdentityId'
  { -- | The error code indicating the type of error that occurred.
    errorCode :: Prelude.Maybe CognitoErrorCode,
    -- | A unique identifier in the format REGION:GUID.
    identityId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnprocessedIdentityId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'unprocessedIdentityId_errorCode' - The error code indicating the type of error that occurred.
--
-- 'identityId', 'unprocessedIdentityId_identityId' - A unique identifier in the format REGION:GUID.
newUnprocessedIdentityId ::
  UnprocessedIdentityId
newUnprocessedIdentityId =
  UnprocessedIdentityId'
    { errorCode = Prelude.Nothing,
      identityId = Prelude.Nothing
    }

-- | The error code indicating the type of error that occurred.
unprocessedIdentityId_errorCode :: Lens.Lens' UnprocessedIdentityId (Prelude.Maybe CognitoErrorCode)
unprocessedIdentityId_errorCode = Lens.lens (\UnprocessedIdentityId' {errorCode} -> errorCode) (\s@UnprocessedIdentityId' {} a -> s {errorCode = a} :: UnprocessedIdentityId)

-- | A unique identifier in the format REGION:GUID.
unprocessedIdentityId_identityId :: Lens.Lens' UnprocessedIdentityId (Prelude.Maybe Prelude.Text)
unprocessedIdentityId_identityId = Lens.lens (\UnprocessedIdentityId' {identityId} -> identityId) (\s@UnprocessedIdentityId' {} a -> s {identityId = a} :: UnprocessedIdentityId)

instance Data.FromJSON UnprocessedIdentityId where
  parseJSON =
    Data.withObject
      "UnprocessedIdentityId"
      ( \x ->
          UnprocessedIdentityId'
            Prelude.<$> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "IdentityId")
      )

instance Prelude.Hashable UnprocessedIdentityId where
  hashWithSalt _salt UnprocessedIdentityId' {..} =
    _salt
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` identityId

instance Prelude.NFData UnprocessedIdentityId where
  rnf UnprocessedIdentityId' {..} =
    Prelude.rnf errorCode `Prelude.seq`
      Prelude.rnf identityId
