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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.BatchUpdateFindingsUnprocessedFinding where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsSecurityFindingIdentifier

-- | A finding from a @BatchUpdateFindings@ request that Security Hub was
-- unable to update.
--
-- /See:/ 'newBatchUpdateFindingsUnprocessedFinding' smart constructor.
data BatchUpdateFindingsUnprocessedFinding = BatchUpdateFindingsUnprocessedFinding'
  { -- | The identifier of the finding that was not updated.
    findingIdentifier :: AwsSecurityFindingIdentifier,
    -- | The code associated with the error. Possible values are:
    --
    -- -   @ConcurrentUpdateError@ - Another request attempted to update the
    --     finding while this request was being processed. This error may also
    --     occur if you call
    --     <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_BatchUpdateFindings.html BatchUpdateFindings>
    --     and
    --     <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_BatchImportFindings.html BatchImportFindings>
    --     at the same time.
    --
    -- -   @DuplicatedFindingIdentifier@ - The request included two or more
    --     findings with the same @FindingIdentifier@.
    --
    -- -   @FindingNotFound@ - The @FindingIdentifier@ included in the request
    --     did not match an existing finding.
    --
    -- -   @FindingSizeExceeded@ - The finding size was greater than the
    --     permissible value of 240 KB.
    --
    -- -   @InternalFailure@ - An internal service failure occurred when
    --     updating the finding.
    --
    -- -   @InvalidInput@ - The finding update contained an invalid value that
    --     did not satisfy the
    --     <https://docs.aws.amazon.com/securityhub/latest/userguide/securityhub-findings-format.html Amazon Web Services Security Finding Format>
    --     syntax.
    errorCode :: Prelude.Text,
    -- | The message associated with the error. Possible values are:
    --
    -- -   @Concurrent finding updates detected@
    --
    -- -   @Finding Identifier is duplicated@
    --
    -- -   @Finding Not Found@
    --
    -- -   @Finding size exceeded 240 KB@
    --
    -- -   @Internal service failure@
    --
    -- -   @Invalid Input@
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
-- 'errorCode', 'batchUpdateFindingsUnprocessedFinding_errorCode' - The code associated with the error. Possible values are:
--
-- -   @ConcurrentUpdateError@ - Another request attempted to update the
--     finding while this request was being processed. This error may also
--     occur if you call
--     <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_BatchUpdateFindings.html BatchUpdateFindings>
--     and
--     <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_BatchImportFindings.html BatchImportFindings>
--     at the same time.
--
-- -   @DuplicatedFindingIdentifier@ - The request included two or more
--     findings with the same @FindingIdentifier@.
--
-- -   @FindingNotFound@ - The @FindingIdentifier@ included in the request
--     did not match an existing finding.
--
-- -   @FindingSizeExceeded@ - The finding size was greater than the
--     permissible value of 240 KB.
--
-- -   @InternalFailure@ - An internal service failure occurred when
--     updating the finding.
--
-- -   @InvalidInput@ - The finding update contained an invalid value that
--     did not satisfy the
--     <https://docs.aws.amazon.com/securityhub/latest/userguide/securityhub-findings-format.html Amazon Web Services Security Finding Format>
--     syntax.
--
-- 'errorMessage', 'batchUpdateFindingsUnprocessedFinding_errorMessage' - The message associated with the error. Possible values are:
--
-- -   @Concurrent finding updates detected@
--
-- -   @Finding Identifier is duplicated@
--
-- -   @Finding Not Found@
--
-- -   @Finding size exceeded 240 KB@
--
-- -   @Internal service failure@
--
-- -   @Invalid Input@
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

-- | The code associated with the error. Possible values are:
--
-- -   @ConcurrentUpdateError@ - Another request attempted to update the
--     finding while this request was being processed. This error may also
--     occur if you call
--     <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_BatchUpdateFindings.html BatchUpdateFindings>
--     and
--     <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_BatchImportFindings.html BatchImportFindings>
--     at the same time.
--
-- -   @DuplicatedFindingIdentifier@ - The request included two or more
--     findings with the same @FindingIdentifier@.
--
-- -   @FindingNotFound@ - The @FindingIdentifier@ included in the request
--     did not match an existing finding.
--
-- -   @FindingSizeExceeded@ - The finding size was greater than the
--     permissible value of 240 KB.
--
-- -   @InternalFailure@ - An internal service failure occurred when
--     updating the finding.
--
-- -   @InvalidInput@ - The finding update contained an invalid value that
--     did not satisfy the
--     <https://docs.aws.amazon.com/securityhub/latest/userguide/securityhub-findings-format.html Amazon Web Services Security Finding Format>
--     syntax.
batchUpdateFindingsUnprocessedFinding_errorCode :: Lens.Lens' BatchUpdateFindingsUnprocessedFinding Prelude.Text
batchUpdateFindingsUnprocessedFinding_errorCode = Lens.lens (\BatchUpdateFindingsUnprocessedFinding' {errorCode} -> errorCode) (\s@BatchUpdateFindingsUnprocessedFinding' {} a -> s {errorCode = a} :: BatchUpdateFindingsUnprocessedFinding)

-- | The message associated with the error. Possible values are:
--
-- -   @Concurrent finding updates detected@
--
-- -   @Finding Identifier is duplicated@
--
-- -   @Finding Not Found@
--
-- -   @Finding size exceeded 240 KB@
--
-- -   @Internal service failure@
--
-- -   @Invalid Input@
batchUpdateFindingsUnprocessedFinding_errorMessage :: Lens.Lens' BatchUpdateFindingsUnprocessedFinding Prelude.Text
batchUpdateFindingsUnprocessedFinding_errorMessage = Lens.lens (\BatchUpdateFindingsUnprocessedFinding' {errorMessage} -> errorMessage) (\s@BatchUpdateFindingsUnprocessedFinding' {} a -> s {errorMessage = a} :: BatchUpdateFindingsUnprocessedFinding)

instance
  Data.FromJSON
    BatchUpdateFindingsUnprocessedFinding
  where
  parseJSON =
    Data.withObject
      "BatchUpdateFindingsUnprocessedFinding"
      ( \x ->
          BatchUpdateFindingsUnprocessedFinding'
            Prelude.<$> (x Data..: "FindingIdentifier")
            Prelude.<*> (x Data..: "ErrorCode")
            Prelude.<*> (x Data..: "ErrorMessage")
      )

instance
  Prelude.Hashable
    BatchUpdateFindingsUnprocessedFinding
  where
  hashWithSalt
    _salt
    BatchUpdateFindingsUnprocessedFinding' {..} =
      _salt
        `Prelude.hashWithSalt` findingIdentifier
        `Prelude.hashWithSalt` errorCode
        `Prelude.hashWithSalt` errorMessage

instance
  Prelude.NFData
    BatchUpdateFindingsUnprocessedFinding
  where
  rnf BatchUpdateFindingsUnprocessedFinding' {..} =
    Prelude.rnf findingIdentifier
      `Prelude.seq` Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
