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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Returns information about errors in a
-- BatchAssociateApprovalRuleTemplateWithRepositories operation.
--
-- /See:/ 'newBatchAssociateApprovalRuleTemplateWithRepositoriesError' smart constructor.
data BatchAssociateApprovalRuleTemplateWithRepositoriesError = BatchAssociateApprovalRuleTemplateWithRepositoriesError'
  { -- | The name of the repository where the association was not made.
    repositoryName :: Core.Maybe Core.Text,
    -- | An error message that provides details about why the repository name was
    -- not found or not valid.
    errorMessage :: Core.Maybe Core.Text,
    -- | An error code that specifies whether the repository name was not valid
    -- or not found.
    errorCode :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      errorMessage =
        Core.Nothing,
      errorCode =
        Core.Nothing
    }

-- | The name of the repository where the association was not made.
batchAssociateApprovalRuleTemplateWithRepositoriesError_repositoryName :: Lens.Lens' BatchAssociateApprovalRuleTemplateWithRepositoriesError (Core.Maybe Core.Text)
batchAssociateApprovalRuleTemplateWithRepositoriesError_repositoryName = Lens.lens (\BatchAssociateApprovalRuleTemplateWithRepositoriesError' {repositoryName} -> repositoryName) (\s@BatchAssociateApprovalRuleTemplateWithRepositoriesError' {} a -> s {repositoryName = a} :: BatchAssociateApprovalRuleTemplateWithRepositoriesError)

-- | An error message that provides details about why the repository name was
-- not found or not valid.
batchAssociateApprovalRuleTemplateWithRepositoriesError_errorMessage :: Lens.Lens' BatchAssociateApprovalRuleTemplateWithRepositoriesError (Core.Maybe Core.Text)
batchAssociateApprovalRuleTemplateWithRepositoriesError_errorMessage = Lens.lens (\BatchAssociateApprovalRuleTemplateWithRepositoriesError' {errorMessage} -> errorMessage) (\s@BatchAssociateApprovalRuleTemplateWithRepositoriesError' {} a -> s {errorMessage = a} :: BatchAssociateApprovalRuleTemplateWithRepositoriesError)

-- | An error code that specifies whether the repository name was not valid
-- or not found.
batchAssociateApprovalRuleTemplateWithRepositoriesError_errorCode :: Lens.Lens' BatchAssociateApprovalRuleTemplateWithRepositoriesError (Core.Maybe Core.Text)
batchAssociateApprovalRuleTemplateWithRepositoriesError_errorCode = Lens.lens (\BatchAssociateApprovalRuleTemplateWithRepositoriesError' {errorCode} -> errorCode) (\s@BatchAssociateApprovalRuleTemplateWithRepositoriesError' {} a -> s {errorCode = a} :: BatchAssociateApprovalRuleTemplateWithRepositoriesError)

instance
  Core.FromJSON
    BatchAssociateApprovalRuleTemplateWithRepositoriesError
  where
  parseJSON =
    Core.withObject
      "BatchAssociateApprovalRuleTemplateWithRepositoriesError"
      ( \x ->
          BatchAssociateApprovalRuleTemplateWithRepositoriesError'
            Core.<$> (x Core..:? "repositoryName")
              Core.<*> (x Core..:? "errorMessage")
              Core.<*> (x Core..:? "errorCode")
      )

instance
  Core.Hashable
    BatchAssociateApprovalRuleTemplateWithRepositoriesError

instance
  Core.NFData
    BatchAssociateApprovalRuleTemplateWithRepositoriesError
