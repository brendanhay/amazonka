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
-- Module      : Amazonka.CodeCommit.Types.BatchGetCommitsError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.BatchGetCommitsError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Returns information about errors in a BatchGetCommits operation.
--
-- /See:/ 'newBatchGetCommitsError' smart constructor.
data BatchGetCommitsError = BatchGetCommitsError'
  { -- | A commit ID that either could not be found or was not in a valid format.
    commitId :: Prelude.Maybe Prelude.Text,
    -- | An error message that provides detail about why the commit ID either was
    -- not found or was not valid.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | An error code that specifies whether the commit ID was not valid or not
    -- found.
    errorCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetCommitsError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'commitId', 'batchGetCommitsError_commitId' - A commit ID that either could not be found or was not in a valid format.
--
-- 'errorMessage', 'batchGetCommitsError_errorMessage' - An error message that provides detail about why the commit ID either was
-- not found or was not valid.
--
-- 'errorCode', 'batchGetCommitsError_errorCode' - An error code that specifies whether the commit ID was not valid or not
-- found.
newBatchGetCommitsError ::
  BatchGetCommitsError
newBatchGetCommitsError =
  BatchGetCommitsError'
    { commitId = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | A commit ID that either could not be found or was not in a valid format.
batchGetCommitsError_commitId :: Lens.Lens' BatchGetCommitsError (Prelude.Maybe Prelude.Text)
batchGetCommitsError_commitId = Lens.lens (\BatchGetCommitsError' {commitId} -> commitId) (\s@BatchGetCommitsError' {} a -> s {commitId = a} :: BatchGetCommitsError)

-- | An error message that provides detail about why the commit ID either was
-- not found or was not valid.
batchGetCommitsError_errorMessage :: Lens.Lens' BatchGetCommitsError (Prelude.Maybe Prelude.Text)
batchGetCommitsError_errorMessage = Lens.lens (\BatchGetCommitsError' {errorMessage} -> errorMessage) (\s@BatchGetCommitsError' {} a -> s {errorMessage = a} :: BatchGetCommitsError)

-- | An error code that specifies whether the commit ID was not valid or not
-- found.
batchGetCommitsError_errorCode :: Lens.Lens' BatchGetCommitsError (Prelude.Maybe Prelude.Text)
batchGetCommitsError_errorCode = Lens.lens (\BatchGetCommitsError' {errorCode} -> errorCode) (\s@BatchGetCommitsError' {} a -> s {errorCode = a} :: BatchGetCommitsError)

instance Core.FromJSON BatchGetCommitsError where
  parseJSON =
    Core.withObject
      "BatchGetCommitsError"
      ( \x ->
          BatchGetCommitsError'
            Prelude.<$> (x Core..:? "commitId")
            Prelude.<*> (x Core..:? "errorMessage")
            Prelude.<*> (x Core..:? "errorCode")
      )

instance Prelude.Hashable BatchGetCommitsError where
  hashWithSalt _salt BatchGetCommitsError' {..} =
    _salt `Prelude.hashWithSalt` commitId
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` errorCode

instance Prelude.NFData BatchGetCommitsError where
  rnf BatchGetCommitsError' {..} =
    Prelude.rnf commitId
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf errorCode
