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
-- Module      : Network.AWS.CodeCommit.Types.BatchGetCommitsError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.BatchGetCommitsError where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON BatchGetCommitsError where
  parseJSON =
    Prelude.withObject
      "BatchGetCommitsError"
      ( \x ->
          BatchGetCommitsError'
            Prelude.<$> (x Prelude..:? "commitId")
            Prelude.<*> (x Prelude..:? "errorMessage")
            Prelude.<*> (x Prelude..:? "errorCode")
      )

instance Prelude.Hashable BatchGetCommitsError

instance Prelude.NFData BatchGetCommitsError
