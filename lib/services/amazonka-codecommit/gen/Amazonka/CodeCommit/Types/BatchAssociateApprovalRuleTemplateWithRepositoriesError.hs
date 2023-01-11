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
-- Module      : Amazonka.CodeCommit.Types.BatchAssociateApprovalRuleTemplateWithRepositoriesError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.BatchAssociateApprovalRuleTemplateWithRepositoriesError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns information about errors in a
-- BatchAssociateApprovalRuleTemplateWithRepositories operation.
--
-- /See:/ 'newBatchAssociateApprovalRuleTemplateWithRepositoriesError' smart constructor.
data BatchAssociateApprovalRuleTemplateWithRepositoriesError = BatchAssociateApprovalRuleTemplateWithRepositoriesError'
  { -- | An error code that specifies whether the repository name was not valid
    -- or not found.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | An error message that provides details about why the repository name was
    -- not found or not valid.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository where the association was not made.
    repositoryName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchAssociateApprovalRuleTemplateWithRepositoriesError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'batchAssociateApprovalRuleTemplateWithRepositoriesError_errorCode' - An error code that specifies whether the repository name was not valid
-- or not found.
--
-- 'errorMessage', 'batchAssociateApprovalRuleTemplateWithRepositoriesError_errorMessage' - An error message that provides details about why the repository name was
-- not found or not valid.
--
-- 'repositoryName', 'batchAssociateApprovalRuleTemplateWithRepositoriesError_repositoryName' - The name of the repository where the association was not made.
newBatchAssociateApprovalRuleTemplateWithRepositoriesError ::
  BatchAssociateApprovalRuleTemplateWithRepositoriesError
newBatchAssociateApprovalRuleTemplateWithRepositoriesError =
  BatchAssociateApprovalRuleTemplateWithRepositoriesError'
    { errorCode =
        Prelude.Nothing,
      errorMessage =
        Prelude.Nothing,
      repositoryName =
        Prelude.Nothing
    }

-- | An error code that specifies whether the repository name was not valid
-- or not found.
batchAssociateApprovalRuleTemplateWithRepositoriesError_errorCode :: Lens.Lens' BatchAssociateApprovalRuleTemplateWithRepositoriesError (Prelude.Maybe Prelude.Text)
batchAssociateApprovalRuleTemplateWithRepositoriesError_errorCode = Lens.lens (\BatchAssociateApprovalRuleTemplateWithRepositoriesError' {errorCode} -> errorCode) (\s@BatchAssociateApprovalRuleTemplateWithRepositoriesError' {} a -> s {errorCode = a} :: BatchAssociateApprovalRuleTemplateWithRepositoriesError)

-- | An error message that provides details about why the repository name was
-- not found or not valid.
batchAssociateApprovalRuleTemplateWithRepositoriesError_errorMessage :: Lens.Lens' BatchAssociateApprovalRuleTemplateWithRepositoriesError (Prelude.Maybe Prelude.Text)
batchAssociateApprovalRuleTemplateWithRepositoriesError_errorMessage = Lens.lens (\BatchAssociateApprovalRuleTemplateWithRepositoriesError' {errorMessage} -> errorMessage) (\s@BatchAssociateApprovalRuleTemplateWithRepositoriesError' {} a -> s {errorMessage = a} :: BatchAssociateApprovalRuleTemplateWithRepositoriesError)

-- | The name of the repository where the association was not made.
batchAssociateApprovalRuleTemplateWithRepositoriesError_repositoryName :: Lens.Lens' BatchAssociateApprovalRuleTemplateWithRepositoriesError (Prelude.Maybe Prelude.Text)
batchAssociateApprovalRuleTemplateWithRepositoriesError_repositoryName = Lens.lens (\BatchAssociateApprovalRuleTemplateWithRepositoriesError' {repositoryName} -> repositoryName) (\s@BatchAssociateApprovalRuleTemplateWithRepositoriesError' {} a -> s {repositoryName = a} :: BatchAssociateApprovalRuleTemplateWithRepositoriesError)

instance
  Data.FromJSON
    BatchAssociateApprovalRuleTemplateWithRepositoriesError
  where
  parseJSON =
    Data.withObject
      "BatchAssociateApprovalRuleTemplateWithRepositoriesError"
      ( \x ->
          BatchAssociateApprovalRuleTemplateWithRepositoriesError'
            Prelude.<$> (x Data..:? "errorCode")
              Prelude.<*> (x Data..:? "errorMessage")
              Prelude.<*> (x Data..:? "repositoryName")
      )

instance
  Prelude.Hashable
    BatchAssociateApprovalRuleTemplateWithRepositoriesError
  where
  hashWithSalt
    _salt
    BatchAssociateApprovalRuleTemplateWithRepositoriesError' {..} =
      _salt `Prelude.hashWithSalt` errorCode
        `Prelude.hashWithSalt` errorMessage
        `Prelude.hashWithSalt` repositoryName

instance
  Prelude.NFData
    BatchAssociateApprovalRuleTemplateWithRepositoriesError
  where
  rnf
    BatchAssociateApprovalRuleTemplateWithRepositoriesError' {..} =
      Prelude.rnf errorCode
        `Prelude.seq` Prelude.rnf errorMessage
        `Prelude.seq` Prelude.rnf repositoryName
