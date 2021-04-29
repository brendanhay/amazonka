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
-- Module      : Network.AWS.CodeCommit.Types.BatchAssociateApprovalRuleTemplateWithRepositoriesError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.BatchAssociateApprovalRuleTemplateWithRepositoriesError where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Returns information about errors in a
-- BatchAssociateApprovalRuleTemplateWithRepositories operation.
--
-- /See:/ 'newBatchAssociateApprovalRuleTemplateWithRepositoriesError' smart constructor.
data BatchAssociateApprovalRuleTemplateWithRepositoriesError = BatchAssociateApprovalRuleTemplateWithRepositoriesError'
  { -- | The name of the repository where the association was not made.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | An error message that provides details about why the repository name was
    -- not found or not valid.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | An error code that specifies whether the repository name was not valid
    -- or not found.
    errorCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchAssociateApprovalRuleTemplateWithRepositoriesError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoryName', 'batchAssociateApprovalRuleTemplateWithRepositoriesError_repositoryName' - The name of the repository where the association was not made.
--
-- 'errorMessage', 'batchAssociateApprovalRuleTemplateWithRepositoriesError_errorMessage' - An error message that provides details about why the repository name was
-- not found or not valid.
--
-- 'errorCode', 'batchAssociateApprovalRuleTemplateWithRepositoriesError_errorCode' - An error code that specifies whether the repository name was not valid
-- or not found.
newBatchAssociateApprovalRuleTemplateWithRepositoriesError ::
  BatchAssociateApprovalRuleTemplateWithRepositoriesError
newBatchAssociateApprovalRuleTemplateWithRepositoriesError =
  BatchAssociateApprovalRuleTemplateWithRepositoriesError'
    { repositoryName =
        Prelude.Nothing,
      errorMessage =
        Prelude.Nothing,
      errorCode =
        Prelude.Nothing
    }

-- | The name of the repository where the association was not made.
batchAssociateApprovalRuleTemplateWithRepositoriesError_repositoryName :: Lens.Lens' BatchAssociateApprovalRuleTemplateWithRepositoriesError (Prelude.Maybe Prelude.Text)
batchAssociateApprovalRuleTemplateWithRepositoriesError_repositoryName = Lens.lens (\BatchAssociateApprovalRuleTemplateWithRepositoriesError' {repositoryName} -> repositoryName) (\s@BatchAssociateApprovalRuleTemplateWithRepositoriesError' {} a -> s {repositoryName = a} :: BatchAssociateApprovalRuleTemplateWithRepositoriesError)

-- | An error message that provides details about why the repository name was
-- not found or not valid.
batchAssociateApprovalRuleTemplateWithRepositoriesError_errorMessage :: Lens.Lens' BatchAssociateApprovalRuleTemplateWithRepositoriesError (Prelude.Maybe Prelude.Text)
batchAssociateApprovalRuleTemplateWithRepositoriesError_errorMessage = Lens.lens (\BatchAssociateApprovalRuleTemplateWithRepositoriesError' {errorMessage} -> errorMessage) (\s@BatchAssociateApprovalRuleTemplateWithRepositoriesError' {} a -> s {errorMessage = a} :: BatchAssociateApprovalRuleTemplateWithRepositoriesError)

-- | An error code that specifies whether the repository name was not valid
-- or not found.
batchAssociateApprovalRuleTemplateWithRepositoriesError_errorCode :: Lens.Lens' BatchAssociateApprovalRuleTemplateWithRepositoriesError (Prelude.Maybe Prelude.Text)
batchAssociateApprovalRuleTemplateWithRepositoriesError_errorCode = Lens.lens (\BatchAssociateApprovalRuleTemplateWithRepositoriesError' {errorCode} -> errorCode) (\s@BatchAssociateApprovalRuleTemplateWithRepositoriesError' {} a -> s {errorCode = a} :: BatchAssociateApprovalRuleTemplateWithRepositoriesError)

instance
  Prelude.FromJSON
    BatchAssociateApprovalRuleTemplateWithRepositoriesError
  where
  parseJSON =
    Prelude.withObject
      "BatchAssociateApprovalRuleTemplateWithRepositoriesError"
      ( \x ->
          BatchAssociateApprovalRuleTemplateWithRepositoriesError'
            Prelude.<$> (x Prelude..:? "repositoryName")
              Prelude.<*> (x Prelude..:? "errorMessage")
              Prelude.<*> (x Prelude..:? "errorCode")
      )

instance
  Prelude.Hashable
    BatchAssociateApprovalRuleTemplateWithRepositoriesError

instance
  Prelude.NFData
    BatchAssociateApprovalRuleTemplateWithRepositoriesError
