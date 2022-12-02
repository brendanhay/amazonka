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
-- Module      : Amazonka.Athena.Types.UnprocessedQueryExecutionId
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.UnprocessedQueryExecutionId where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a query execution that failed to process.
--
-- /See:/ 'newUnprocessedQueryExecutionId' smart constructor.
data UnprocessedQueryExecutionId = UnprocessedQueryExecutionId'
  { -- | The unique identifier of the query execution.
    queryExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The error message returned when the query execution failed to process,
    -- if applicable.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The error code returned when the query execution failed to process, if
    -- applicable.
    errorCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnprocessedQueryExecutionId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryExecutionId', 'unprocessedQueryExecutionId_queryExecutionId' - The unique identifier of the query execution.
--
-- 'errorMessage', 'unprocessedQueryExecutionId_errorMessage' - The error message returned when the query execution failed to process,
-- if applicable.
--
-- 'errorCode', 'unprocessedQueryExecutionId_errorCode' - The error code returned when the query execution failed to process, if
-- applicable.
newUnprocessedQueryExecutionId ::
  UnprocessedQueryExecutionId
newUnprocessedQueryExecutionId =
  UnprocessedQueryExecutionId'
    { queryExecutionId =
        Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | The unique identifier of the query execution.
unprocessedQueryExecutionId_queryExecutionId :: Lens.Lens' UnprocessedQueryExecutionId (Prelude.Maybe Prelude.Text)
unprocessedQueryExecutionId_queryExecutionId = Lens.lens (\UnprocessedQueryExecutionId' {queryExecutionId} -> queryExecutionId) (\s@UnprocessedQueryExecutionId' {} a -> s {queryExecutionId = a} :: UnprocessedQueryExecutionId)

-- | The error message returned when the query execution failed to process,
-- if applicable.
unprocessedQueryExecutionId_errorMessage :: Lens.Lens' UnprocessedQueryExecutionId (Prelude.Maybe Prelude.Text)
unprocessedQueryExecutionId_errorMessage = Lens.lens (\UnprocessedQueryExecutionId' {errorMessage} -> errorMessage) (\s@UnprocessedQueryExecutionId' {} a -> s {errorMessage = a} :: UnprocessedQueryExecutionId)

-- | The error code returned when the query execution failed to process, if
-- applicable.
unprocessedQueryExecutionId_errorCode :: Lens.Lens' UnprocessedQueryExecutionId (Prelude.Maybe Prelude.Text)
unprocessedQueryExecutionId_errorCode = Lens.lens (\UnprocessedQueryExecutionId' {errorCode} -> errorCode) (\s@UnprocessedQueryExecutionId' {} a -> s {errorCode = a} :: UnprocessedQueryExecutionId)

instance Data.FromJSON UnprocessedQueryExecutionId where
  parseJSON =
    Data.withObject
      "UnprocessedQueryExecutionId"
      ( \x ->
          UnprocessedQueryExecutionId'
            Prelude.<$> (x Data..:? "QueryExecutionId")
            Prelude.<*> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "ErrorCode")
      )

instance Prelude.Hashable UnprocessedQueryExecutionId where
  hashWithSalt _salt UnprocessedQueryExecutionId' {..} =
    _salt `Prelude.hashWithSalt` queryExecutionId
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` errorCode

instance Prelude.NFData UnprocessedQueryExecutionId where
  rnf UnprocessedQueryExecutionId' {..} =
    Prelude.rnf queryExecutionId
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf errorCode
