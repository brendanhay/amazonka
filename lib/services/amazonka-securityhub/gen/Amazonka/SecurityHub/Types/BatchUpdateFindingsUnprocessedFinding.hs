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
-- Module      : Amazonka.SecurityHub.Types.BatchUpdateFindingsUnprocessedFinding
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.BatchUpdateFindingsUnprocessedFinding where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsSecurityFindingIdentifier

-- | A finding from a @BatchUpdateFindings@ request that Security Hub was
-- unable to update.
--
-- /See:/ 'newBatchUpdateFindingsUnprocessedFinding' smart constructor.
data BatchUpdateFindingsUnprocessedFinding = BatchUpdateFindingsUnprocessedFinding'
  { -- | The identifier of the finding that was not updated.
    findingIdentifier :: AwsSecurityFindingIdentifier,
    -- | The code associated with the error.
    errorCode :: Prelude.Text,
    -- | The message associated with the error.
    errorMessage :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchUpdateFindingsUnprocessedFinding' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'findingIdentifier', 'batchUpdateFindingsUnprocessedFinding_findingIdentifier' - The identifier of the finding that was not updated.
--
-- 'errorCode', 'batchUpdateFindingsUnprocessedFinding_errorCode' - The code associated with the error.
--
-- 'errorMessage', 'batchUpdateFindingsUnprocessedFinding_errorMessage' - The message associated with the error.
newBatchUpdateFindingsUnprocessedFinding ::
  -- | 'findingIdentifier'
  AwsSecurityFindingIdentifier ->
  -- | 'errorCode'
  Prelude.Text ->
  -- | 'errorMessage'
  Prelude.Text ->
  BatchUpdateFindingsUnprocessedFinding
newBatchUpdateFindingsUnprocessedFinding
  pFindingIdentifier_
  pErrorCode_
  pErrorMessage_ =
    BatchUpdateFindingsUnprocessedFinding'
      { findingIdentifier =
          pFindingIdentifier_,
        errorCode = pErrorCode_,
        errorMessage = pErrorMessage_
      }

-- | The identifier of the finding that was not updated.
batchUpdateFindingsUnprocessedFinding_findingIdentifier :: Lens.Lens' BatchUpdateFindingsUnprocessedFinding AwsSecurityFindingIdentifier
batchUpdateFindingsUnprocessedFinding_findingIdentifier = Lens.lens (\BatchUpdateFindingsUnprocessedFinding' {findingIdentifier} -> findingIdentifier) (\s@BatchUpdateFindingsUnprocessedFinding' {} a -> s {findingIdentifier = a} :: BatchUpdateFindingsUnprocessedFinding)

-- | The code associated with the error.
batchUpdateFindingsUnprocessedFinding_errorCode :: Lens.Lens' BatchUpdateFindingsUnprocessedFinding Prelude.Text
batchUpdateFindingsUnprocessedFinding_errorCode = Lens.lens (\BatchUpdateFindingsUnprocessedFinding' {errorCode} -> errorCode) (\s@BatchUpdateFindingsUnprocessedFinding' {} a -> s {errorCode = a} :: BatchUpdateFindingsUnprocessedFinding)

-- | The message associated with the error.
batchUpdateFindingsUnprocessedFinding_errorMessage :: Lens.Lens' BatchUpdateFindingsUnprocessedFinding Prelude.Text
batchUpdateFindingsUnprocessedFinding_errorMessage = Lens.lens (\BatchUpdateFindingsUnprocessedFinding' {errorMessage} -> errorMessage) (\s@BatchUpdateFindingsUnprocessedFinding' {} a -> s {errorMessage = a} :: BatchUpdateFindingsUnprocessedFinding)

instance
  Core.FromJSON
    BatchUpdateFindingsUnprocessedFinding
  where
  parseJSON =
    Core.withObject
      "BatchUpdateFindingsUnprocessedFinding"
      ( \x ->
          BatchUpdateFindingsUnprocessedFinding'
            Prelude.<$> (x Core..: "FindingIdentifier")
            Prelude.<*> (x Core..: "ErrorCode")
            Prelude.<*> (x Core..: "ErrorMessage")
      )

instance
  Prelude.Hashable
    BatchUpdateFindingsUnprocessedFinding

instance
  Prelude.NFData
    BatchUpdateFindingsUnprocessedFinding
