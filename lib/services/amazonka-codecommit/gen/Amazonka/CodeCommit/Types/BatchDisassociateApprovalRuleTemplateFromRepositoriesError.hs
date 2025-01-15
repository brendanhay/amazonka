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
-- Module      : Amazonka.CodeCommit.Types.BatchDisassociateApprovalRuleTemplateFromRepositoriesError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.BatchDisassociateApprovalRuleTemplateFromRepositoriesError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns information about errors in a
-- BatchDisassociateApprovalRuleTemplateFromRepositories operation.
--
-- /See:/ 'newBatchDisassociateApprovalRuleTemplateFromRepositoriesError' smart constructor.
data BatchDisassociateApprovalRuleTemplateFromRepositoriesError = BatchDisassociateApprovalRuleTemplateFromRepositoriesError'
  { -- | An error code that specifies whether the repository name was not valid
    -- or not found.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | An error message that provides details about why the repository name was
    -- either not found or not valid.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository where the association with the template was
    -- not able to be removed.
    repositoryName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDisassociateApprovalRuleTemplateFromRepositoriesError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'batchDisassociateApprovalRuleTemplateFromRepositoriesError_errorCode' - An error code that specifies whether the repository name was not valid
-- or not found.
--
-- 'errorMessage', 'batchDisassociateApprovalRuleTemplateFromRepositoriesError_errorMessage' - An error message that provides details about why the repository name was
-- either not found or not valid.
--
-- 'repositoryName', 'batchDisassociateApprovalRuleTemplateFromRepositoriesError_repositoryName' - The name of the repository where the association with the template was
-- not able to be removed.
newBatchDisassociateApprovalRuleTemplateFromRepositoriesError ::
  BatchDisassociateApprovalRuleTemplateFromRepositoriesError
newBatchDisassociateApprovalRuleTemplateFromRepositoriesError =
  BatchDisassociateApprovalRuleTemplateFromRepositoriesError'
    { errorCode =
        Prelude.Nothing,
      errorMessage =
        Prelude.Nothing,
      repositoryName =
        Prelude.Nothing
    }

-- | An error code that specifies whether the repository name was not valid
-- or not found.
batchDisassociateApprovalRuleTemplateFromRepositoriesError_errorCode :: Lens.Lens' BatchDisassociateApprovalRuleTemplateFromRepositoriesError (Prelude.Maybe Prelude.Text)
batchDisassociateApprovalRuleTemplateFromRepositoriesError_errorCode = Lens.lens (\BatchDisassociateApprovalRuleTemplateFromRepositoriesError' {errorCode} -> errorCode) (\s@BatchDisassociateApprovalRuleTemplateFromRepositoriesError' {} a -> s {errorCode = a} :: BatchDisassociateApprovalRuleTemplateFromRepositoriesError)

-- | An error message that provides details about why the repository name was
-- either not found or not valid.
batchDisassociateApprovalRuleTemplateFromRepositoriesError_errorMessage :: Lens.Lens' BatchDisassociateApprovalRuleTemplateFromRepositoriesError (Prelude.Maybe Prelude.Text)
batchDisassociateApprovalRuleTemplateFromRepositoriesError_errorMessage = Lens.lens (\BatchDisassociateApprovalRuleTemplateFromRepositoriesError' {errorMessage} -> errorMessage) (\s@BatchDisassociateApprovalRuleTemplateFromRepositoriesError' {} a -> s {errorMessage = a} :: BatchDisassociateApprovalRuleTemplateFromRepositoriesError)

-- | The name of the repository where the association with the template was
-- not able to be removed.
batchDisassociateApprovalRuleTemplateFromRepositoriesError_repositoryName :: Lens.Lens' BatchDisassociateApprovalRuleTemplateFromRepositoriesError (Prelude.Maybe Prelude.Text)
batchDisassociateApprovalRuleTemplateFromRepositoriesError_repositoryName = Lens.lens (\BatchDisassociateApprovalRuleTemplateFromRepositoriesError' {repositoryName} -> repositoryName) (\s@BatchDisassociateApprovalRuleTemplateFromRepositoriesError' {} a -> s {repositoryName = a} :: BatchDisassociateApprovalRuleTemplateFromRepositoriesError)

instance
  Data.FromJSON
    BatchDisassociateApprovalRuleTemplateFromRepositoriesError
  where
  parseJSON =
    Data.withObject
      "BatchDisassociateApprovalRuleTemplateFromRepositoriesError"
      ( \x ->
          BatchDisassociateApprovalRuleTemplateFromRepositoriesError'
            Prelude.<$> (x Data..:? "errorCode")
            Prelude.<*> (x Data..:? "errorMessage")
            Prelude.<*> (x Data..:? "repositoryName")
      )

instance
  Prelude.Hashable
    BatchDisassociateApprovalRuleTemplateFromRepositoriesError
  where
  hashWithSalt
    _salt
    BatchDisassociateApprovalRuleTemplateFromRepositoriesError' {..} =
      _salt
        `Prelude.hashWithSalt` errorCode
        `Prelude.hashWithSalt` errorMessage
        `Prelude.hashWithSalt` repositoryName

instance
  Prelude.NFData
    BatchDisassociateApprovalRuleTemplateFromRepositoriesError
  where
  rnf
    BatchDisassociateApprovalRuleTemplateFromRepositoriesError' {..} =
      Prelude.rnf errorCode `Prelude.seq`
        Prelude.rnf errorMessage `Prelude.seq`
          Prelude.rnf repositoryName
