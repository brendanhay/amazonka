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
-- Module      : Network.AWS.CodeCommit.Types.BatchDisassociateApprovalRuleTemplateFromRepositoriesError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.BatchDisassociateApprovalRuleTemplateFromRepositoriesError where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Returns information about errors in a
-- BatchDisassociateApprovalRuleTemplateFromRepositories operation.
--
-- /See:/ 'newBatchDisassociateApprovalRuleTemplateFromRepositoriesError' smart constructor.
data BatchDisassociateApprovalRuleTemplateFromRepositoriesError = BatchDisassociateApprovalRuleTemplateFromRepositoriesError'
  { -- | The name of the repository where the association with the template was
    -- not able to be removed.
    repositoryName :: Core.Maybe Core.Text,
    -- | An error message that provides details about why the repository name was
    -- either not found or not valid.
    errorMessage :: Core.Maybe Core.Text,
    -- | An error code that specifies whether the repository name was not valid
    -- or not found.
    errorCode :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchDisassociateApprovalRuleTemplateFromRepositoriesError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoryName', 'batchDisassociateApprovalRuleTemplateFromRepositoriesError_repositoryName' - The name of the repository where the association with the template was
-- not able to be removed.
--
-- 'errorMessage', 'batchDisassociateApprovalRuleTemplateFromRepositoriesError_errorMessage' - An error message that provides details about why the repository name was
-- either not found or not valid.
--
-- 'errorCode', 'batchDisassociateApprovalRuleTemplateFromRepositoriesError_errorCode' - An error code that specifies whether the repository name was not valid
-- or not found.
newBatchDisassociateApprovalRuleTemplateFromRepositoriesError ::
  BatchDisassociateApprovalRuleTemplateFromRepositoriesError
newBatchDisassociateApprovalRuleTemplateFromRepositoriesError =
  BatchDisassociateApprovalRuleTemplateFromRepositoriesError'
    { repositoryName =
        Core.Nothing,
      errorMessage =
        Core.Nothing,
      errorCode =
        Core.Nothing
    }

-- | The name of the repository where the association with the template was
-- not able to be removed.
batchDisassociateApprovalRuleTemplateFromRepositoriesError_repositoryName :: Lens.Lens' BatchDisassociateApprovalRuleTemplateFromRepositoriesError (Core.Maybe Core.Text)
batchDisassociateApprovalRuleTemplateFromRepositoriesError_repositoryName = Lens.lens (\BatchDisassociateApprovalRuleTemplateFromRepositoriesError' {repositoryName} -> repositoryName) (\s@BatchDisassociateApprovalRuleTemplateFromRepositoriesError' {} a -> s {repositoryName = a} :: BatchDisassociateApprovalRuleTemplateFromRepositoriesError)

-- | An error message that provides details about why the repository name was
-- either not found or not valid.
batchDisassociateApprovalRuleTemplateFromRepositoriesError_errorMessage :: Lens.Lens' BatchDisassociateApprovalRuleTemplateFromRepositoriesError (Core.Maybe Core.Text)
batchDisassociateApprovalRuleTemplateFromRepositoriesError_errorMessage = Lens.lens (\BatchDisassociateApprovalRuleTemplateFromRepositoriesError' {errorMessage} -> errorMessage) (\s@BatchDisassociateApprovalRuleTemplateFromRepositoriesError' {} a -> s {errorMessage = a} :: BatchDisassociateApprovalRuleTemplateFromRepositoriesError)

-- | An error code that specifies whether the repository name was not valid
-- or not found.
batchDisassociateApprovalRuleTemplateFromRepositoriesError_errorCode :: Lens.Lens' BatchDisassociateApprovalRuleTemplateFromRepositoriesError (Core.Maybe Core.Text)
batchDisassociateApprovalRuleTemplateFromRepositoriesError_errorCode = Lens.lens (\BatchDisassociateApprovalRuleTemplateFromRepositoriesError' {errorCode} -> errorCode) (\s@BatchDisassociateApprovalRuleTemplateFromRepositoriesError' {} a -> s {errorCode = a} :: BatchDisassociateApprovalRuleTemplateFromRepositoriesError)

instance
  Core.FromJSON
    BatchDisassociateApprovalRuleTemplateFromRepositoriesError
  where
  parseJSON =
    Core.withObject
      "BatchDisassociateApprovalRuleTemplateFromRepositoriesError"
      ( \x ->
          BatchDisassociateApprovalRuleTemplateFromRepositoriesError'
            Core.<$> (x Core..:? "repositoryName")
              Core.<*> (x Core..:? "errorMessage")
              Core.<*> (x Core..:? "errorCode")
      )

instance
  Core.Hashable
    BatchDisassociateApprovalRuleTemplateFromRepositoriesError

instance
  Core.NFData
    BatchDisassociateApprovalRuleTemplateFromRepositoriesError
