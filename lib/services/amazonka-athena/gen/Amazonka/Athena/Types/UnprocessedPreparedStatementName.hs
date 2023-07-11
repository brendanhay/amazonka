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
-- Module      : Amazonka.Athena.Types.UnprocessedPreparedStatementName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.UnprocessedPreparedStatementName where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The name of a prepared statement that could not be returned.
--
-- /See:/ 'newUnprocessedPreparedStatementName' smart constructor.
data UnprocessedPreparedStatementName = UnprocessedPreparedStatementName'
  { -- | The error code returned when the request for the prepared statement
    -- failed.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The error message containing the reason why the prepared statement could
    -- not be returned. The following error messages are possible:
    --
    -- -   @INVALID_INPUT@ - The name of the prepared statement that was
    --     provided is not valid (for example, the name is too long).
    --
    -- -   @STATEMENT_NOT_FOUND@ - A prepared statement with the name provided
    --     could not be found.
    --
    -- -   @UNAUTHORIZED@ - The requester does not have permission to access
    --     the workgroup that contains the prepared statement.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The name of a prepared statement that could not be returned due to an
    -- error.
    statementName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnprocessedPreparedStatementName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'unprocessedPreparedStatementName_errorCode' - The error code returned when the request for the prepared statement
-- failed.
--
-- 'errorMessage', 'unprocessedPreparedStatementName_errorMessage' - The error message containing the reason why the prepared statement could
-- not be returned. The following error messages are possible:
--
-- -   @INVALID_INPUT@ - The name of the prepared statement that was
--     provided is not valid (for example, the name is too long).
--
-- -   @STATEMENT_NOT_FOUND@ - A prepared statement with the name provided
--     could not be found.
--
-- -   @UNAUTHORIZED@ - The requester does not have permission to access
--     the workgroup that contains the prepared statement.
--
-- 'statementName', 'unprocessedPreparedStatementName_statementName' - The name of a prepared statement that could not be returned due to an
-- error.
newUnprocessedPreparedStatementName ::
  UnprocessedPreparedStatementName
newUnprocessedPreparedStatementName =
  UnprocessedPreparedStatementName'
    { errorCode =
        Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      statementName = Prelude.Nothing
    }

-- | The error code returned when the request for the prepared statement
-- failed.
unprocessedPreparedStatementName_errorCode :: Lens.Lens' UnprocessedPreparedStatementName (Prelude.Maybe Prelude.Text)
unprocessedPreparedStatementName_errorCode = Lens.lens (\UnprocessedPreparedStatementName' {errorCode} -> errorCode) (\s@UnprocessedPreparedStatementName' {} a -> s {errorCode = a} :: UnprocessedPreparedStatementName)

-- | The error message containing the reason why the prepared statement could
-- not be returned. The following error messages are possible:
--
-- -   @INVALID_INPUT@ - The name of the prepared statement that was
--     provided is not valid (for example, the name is too long).
--
-- -   @STATEMENT_NOT_FOUND@ - A prepared statement with the name provided
--     could not be found.
--
-- -   @UNAUTHORIZED@ - The requester does not have permission to access
--     the workgroup that contains the prepared statement.
unprocessedPreparedStatementName_errorMessage :: Lens.Lens' UnprocessedPreparedStatementName (Prelude.Maybe Prelude.Text)
unprocessedPreparedStatementName_errorMessage = Lens.lens (\UnprocessedPreparedStatementName' {errorMessage} -> errorMessage) (\s@UnprocessedPreparedStatementName' {} a -> s {errorMessage = a} :: UnprocessedPreparedStatementName)

-- | The name of a prepared statement that could not be returned due to an
-- error.
unprocessedPreparedStatementName_statementName :: Lens.Lens' UnprocessedPreparedStatementName (Prelude.Maybe Prelude.Text)
unprocessedPreparedStatementName_statementName = Lens.lens (\UnprocessedPreparedStatementName' {statementName} -> statementName) (\s@UnprocessedPreparedStatementName' {} a -> s {statementName = a} :: UnprocessedPreparedStatementName)

instance
  Data.FromJSON
    UnprocessedPreparedStatementName
  where
  parseJSON =
    Data.withObject
      "UnprocessedPreparedStatementName"
      ( \x ->
          UnprocessedPreparedStatementName'
            Prelude.<$> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "StatementName")
      )

instance
  Prelude.Hashable
    UnprocessedPreparedStatementName
  where
  hashWithSalt
    _salt
    UnprocessedPreparedStatementName' {..} =
      _salt
        `Prelude.hashWithSalt` errorCode
        `Prelude.hashWithSalt` errorMessage
        `Prelude.hashWithSalt` statementName

instance
  Prelude.NFData
    UnprocessedPreparedStatementName
  where
  rnf UnprocessedPreparedStatementName' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf statementName
