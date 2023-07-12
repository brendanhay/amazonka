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
-- Module      : Amazonka.Athena.Types.UnprocessedNamedQueryId
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.UnprocessedNamedQueryId where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a named query ID that could not be processed.
--
-- /See:/ 'newUnprocessedNamedQueryId' smart constructor.
data UnprocessedNamedQueryId = UnprocessedNamedQueryId'
  { -- | The error code returned when the processing request for the named query
    -- failed, if applicable.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The error message returned when the processing request for the named
    -- query failed, if applicable.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the named query.
    namedQueryId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnprocessedNamedQueryId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'unprocessedNamedQueryId_errorCode' - The error code returned when the processing request for the named query
-- failed, if applicable.
--
-- 'errorMessage', 'unprocessedNamedQueryId_errorMessage' - The error message returned when the processing request for the named
-- query failed, if applicable.
--
-- 'namedQueryId', 'unprocessedNamedQueryId_namedQueryId' - The unique identifier of the named query.
newUnprocessedNamedQueryId ::
  UnprocessedNamedQueryId
newUnprocessedNamedQueryId =
  UnprocessedNamedQueryId'
    { errorCode =
        Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      namedQueryId = Prelude.Nothing
    }

-- | The error code returned when the processing request for the named query
-- failed, if applicable.
unprocessedNamedQueryId_errorCode :: Lens.Lens' UnprocessedNamedQueryId (Prelude.Maybe Prelude.Text)
unprocessedNamedQueryId_errorCode = Lens.lens (\UnprocessedNamedQueryId' {errorCode} -> errorCode) (\s@UnprocessedNamedQueryId' {} a -> s {errorCode = a} :: UnprocessedNamedQueryId)

-- | The error message returned when the processing request for the named
-- query failed, if applicable.
unprocessedNamedQueryId_errorMessage :: Lens.Lens' UnprocessedNamedQueryId (Prelude.Maybe Prelude.Text)
unprocessedNamedQueryId_errorMessage = Lens.lens (\UnprocessedNamedQueryId' {errorMessage} -> errorMessage) (\s@UnprocessedNamedQueryId' {} a -> s {errorMessage = a} :: UnprocessedNamedQueryId)

-- | The unique identifier of the named query.
unprocessedNamedQueryId_namedQueryId :: Lens.Lens' UnprocessedNamedQueryId (Prelude.Maybe Prelude.Text)
unprocessedNamedQueryId_namedQueryId = Lens.lens (\UnprocessedNamedQueryId' {namedQueryId} -> namedQueryId) (\s@UnprocessedNamedQueryId' {} a -> s {namedQueryId = a} :: UnprocessedNamedQueryId)

instance Data.FromJSON UnprocessedNamedQueryId where
  parseJSON =
    Data.withObject
      "UnprocessedNamedQueryId"
      ( \x ->
          UnprocessedNamedQueryId'
            Prelude.<$> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "NamedQueryId")
      )

instance Prelude.Hashable UnprocessedNamedQueryId where
  hashWithSalt _salt UnprocessedNamedQueryId' {..} =
    _salt
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` namedQueryId

instance Prelude.NFData UnprocessedNamedQueryId where
  rnf UnprocessedNamedQueryId' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf namedQueryId
