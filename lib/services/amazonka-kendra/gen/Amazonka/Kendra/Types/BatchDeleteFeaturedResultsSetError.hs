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
-- Module      : Amazonka.Kendra.Types.BatchDeleteFeaturedResultsSetError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.BatchDeleteFeaturedResultsSetError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.ErrorCode
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a set of featured results that couldn\'t be
-- removed from an index by the
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_BatchDeleteFeaturedResultsSet.html BatchDeleteFeaturedResultsSet>
-- API.
--
-- /See:/ 'newBatchDeleteFeaturedResultsSetError' smart constructor.
data BatchDeleteFeaturedResultsSetError = BatchDeleteFeaturedResultsSetError'
  { -- | The identifier of the set of featured results that couldn\'t be removed
    -- from the index.
    id :: Prelude.Text,
    -- | The error code for why the set of featured results couldn\'t be removed
    -- from the index.
    errorCode :: ErrorCode,
    -- | An explanation for why the set of featured results couldn\'t be removed
    -- from the index.
    errorMessage :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteFeaturedResultsSetError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'batchDeleteFeaturedResultsSetError_id' - The identifier of the set of featured results that couldn\'t be removed
-- from the index.
--
-- 'errorCode', 'batchDeleteFeaturedResultsSetError_errorCode' - The error code for why the set of featured results couldn\'t be removed
-- from the index.
--
-- 'errorMessage', 'batchDeleteFeaturedResultsSetError_errorMessage' - An explanation for why the set of featured results couldn\'t be removed
-- from the index.
newBatchDeleteFeaturedResultsSetError ::
  -- | 'id'
  Prelude.Text ->
  -- | 'errorCode'
  ErrorCode ->
  -- | 'errorMessage'
  Prelude.Text ->
  BatchDeleteFeaturedResultsSetError
newBatchDeleteFeaturedResultsSetError
  pId_
  pErrorCode_
  pErrorMessage_ =
    BatchDeleteFeaturedResultsSetError'
      { id = pId_,
        errorCode = pErrorCode_,
        errorMessage = pErrorMessage_
      }

-- | The identifier of the set of featured results that couldn\'t be removed
-- from the index.
batchDeleteFeaturedResultsSetError_id :: Lens.Lens' BatchDeleteFeaturedResultsSetError Prelude.Text
batchDeleteFeaturedResultsSetError_id = Lens.lens (\BatchDeleteFeaturedResultsSetError' {id} -> id) (\s@BatchDeleteFeaturedResultsSetError' {} a -> s {id = a} :: BatchDeleteFeaturedResultsSetError)

-- | The error code for why the set of featured results couldn\'t be removed
-- from the index.
batchDeleteFeaturedResultsSetError_errorCode :: Lens.Lens' BatchDeleteFeaturedResultsSetError ErrorCode
batchDeleteFeaturedResultsSetError_errorCode = Lens.lens (\BatchDeleteFeaturedResultsSetError' {errorCode} -> errorCode) (\s@BatchDeleteFeaturedResultsSetError' {} a -> s {errorCode = a} :: BatchDeleteFeaturedResultsSetError)

-- | An explanation for why the set of featured results couldn\'t be removed
-- from the index.
batchDeleteFeaturedResultsSetError_errorMessage :: Lens.Lens' BatchDeleteFeaturedResultsSetError Prelude.Text
batchDeleteFeaturedResultsSetError_errorMessage = Lens.lens (\BatchDeleteFeaturedResultsSetError' {errorMessage} -> errorMessage) (\s@BatchDeleteFeaturedResultsSetError' {} a -> s {errorMessage = a} :: BatchDeleteFeaturedResultsSetError)

instance
  Data.FromJSON
    BatchDeleteFeaturedResultsSetError
  where
  parseJSON =
    Data.withObject
      "BatchDeleteFeaturedResultsSetError"
      ( \x ->
          BatchDeleteFeaturedResultsSetError'
            Prelude.<$> (x Data..: "Id")
            Prelude.<*> (x Data..: "ErrorCode")
            Prelude.<*> (x Data..: "ErrorMessage")
      )

instance
  Prelude.Hashable
    BatchDeleteFeaturedResultsSetError
  where
  hashWithSalt
    _salt
    BatchDeleteFeaturedResultsSetError' {..} =
      _salt
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` errorCode
        `Prelude.hashWithSalt` errorMessage

instance
  Prelude.NFData
    BatchDeleteFeaturedResultsSetError
  where
  rnf BatchDeleteFeaturedResultsSetError' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
