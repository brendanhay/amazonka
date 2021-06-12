{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.BatchDisassociateApprovalRuleTemplateFromRepositories
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the association between an approval rule template and one or
-- more specified repositories.
module Network.AWS.CodeCommit.BatchDisassociateApprovalRuleTemplateFromRepositories
  ( -- * Creating a Request
    BatchDisassociateApprovalRuleTemplateFromRepositories (..),
    newBatchDisassociateApprovalRuleTemplateFromRepositories,

    -- * Request Lenses
    batchDisassociateApprovalRuleTemplateFromRepositories_approvalRuleTemplateName,
    batchDisassociateApprovalRuleTemplateFromRepositories_repositoryNames,

    -- * Destructuring the Response
    BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse (..),
    newBatchDisassociateApprovalRuleTemplateFromRepositoriesResponse,

    -- * Response Lenses
    batchDisassociateApprovalRuleTemplateFromRepositoriesResponse_httpStatus,
    batchDisassociateApprovalRuleTemplateFromRepositoriesResponse_disassociatedRepositoryNames,
    batchDisassociateApprovalRuleTemplateFromRepositoriesResponse_errors,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchDisassociateApprovalRuleTemplateFromRepositories' smart constructor.
data BatchDisassociateApprovalRuleTemplateFromRepositories = BatchDisassociateApprovalRuleTemplateFromRepositories'
  { -- | The name of the template that you want to disassociate from one or more
    -- repositories.
    approvalRuleTemplateName :: Core.Text,
    -- | The repository names that you want to disassociate from the approval
    -- rule template.
    --
    -- The length constraint limit is for each string in the array. The array
    -- itself can be empty.
    repositoryNames :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchDisassociateApprovalRuleTemplateFromRepositories' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'approvalRuleTemplateName', 'batchDisassociateApprovalRuleTemplateFromRepositories_approvalRuleTemplateName' - The name of the template that you want to disassociate from one or more
-- repositories.
--
-- 'repositoryNames', 'batchDisassociateApprovalRuleTemplateFromRepositories_repositoryNames' - The repository names that you want to disassociate from the approval
-- rule template.
--
-- The length constraint limit is for each string in the array. The array
-- itself can be empty.
newBatchDisassociateApprovalRuleTemplateFromRepositories ::
  -- | 'approvalRuleTemplateName'
  Core.Text ->
  BatchDisassociateApprovalRuleTemplateFromRepositories
newBatchDisassociateApprovalRuleTemplateFromRepositories
  pApprovalRuleTemplateName_ =
    BatchDisassociateApprovalRuleTemplateFromRepositories'
      { approvalRuleTemplateName =
          pApprovalRuleTemplateName_,
        repositoryNames =
          Core.mempty
      }

-- | The name of the template that you want to disassociate from one or more
-- repositories.
batchDisassociateApprovalRuleTemplateFromRepositories_approvalRuleTemplateName :: Lens.Lens' BatchDisassociateApprovalRuleTemplateFromRepositories Core.Text
batchDisassociateApprovalRuleTemplateFromRepositories_approvalRuleTemplateName = Lens.lens (\BatchDisassociateApprovalRuleTemplateFromRepositories' {approvalRuleTemplateName} -> approvalRuleTemplateName) (\s@BatchDisassociateApprovalRuleTemplateFromRepositories' {} a -> s {approvalRuleTemplateName = a} :: BatchDisassociateApprovalRuleTemplateFromRepositories)

-- | The repository names that you want to disassociate from the approval
-- rule template.
--
-- The length constraint limit is for each string in the array. The array
-- itself can be empty.
batchDisassociateApprovalRuleTemplateFromRepositories_repositoryNames :: Lens.Lens' BatchDisassociateApprovalRuleTemplateFromRepositories [Core.Text]
batchDisassociateApprovalRuleTemplateFromRepositories_repositoryNames = Lens.lens (\BatchDisassociateApprovalRuleTemplateFromRepositories' {repositoryNames} -> repositoryNames) (\s@BatchDisassociateApprovalRuleTemplateFromRepositories' {} a -> s {repositoryNames = a} :: BatchDisassociateApprovalRuleTemplateFromRepositories) Core.. Lens._Coerce

instance
  Core.AWSRequest
    BatchDisassociateApprovalRuleTemplateFromRepositories
  where
  type
    AWSResponse
      BatchDisassociateApprovalRuleTemplateFromRepositories =
      BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
              Core.<*> ( x Core..?> "disassociatedRepositoryNames"
                           Core..!@ Core.mempty
                       )
              Core.<*> (x Core..?> "errors" Core..!@ Core.mempty)
      )

instance
  Core.Hashable
    BatchDisassociateApprovalRuleTemplateFromRepositories

instance
  Core.NFData
    BatchDisassociateApprovalRuleTemplateFromRepositories

instance
  Core.ToHeaders
    BatchDisassociateApprovalRuleTemplateFromRepositories
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.BatchDisassociateApprovalRuleTemplateFromRepositories" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    BatchDisassociateApprovalRuleTemplateFromRepositories
  where
  toJSON
    BatchDisassociateApprovalRuleTemplateFromRepositories' {..} =
      Core.object
        ( Core.catMaybes
            [ Core.Just
                ( "approvalRuleTemplateName"
                    Core..= approvalRuleTemplateName
                ),
              Core.Just
                ("repositoryNames" Core..= repositoryNames)
            ]
        )

instance
  Core.ToPath
    BatchDisassociateApprovalRuleTemplateFromRepositories
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    BatchDisassociateApprovalRuleTemplateFromRepositories
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newBatchDisassociateApprovalRuleTemplateFromRepositoriesResponse' smart constructor.
data BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse = BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A list of repository names that have had their association with the
    -- template removed.
    disassociatedRepositoryNames :: [Core.Text],
    -- | A list of any errors that might have occurred while attempting to remove
    -- the association between the template and the repositories.
    errors :: [BatchDisassociateApprovalRuleTemplateFromRepositoriesError]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'batchDisassociateApprovalRuleTemplateFromRepositoriesResponse_httpStatus' - The response's http status code.
--
-- 'disassociatedRepositoryNames', 'batchDisassociateApprovalRuleTemplateFromRepositoriesResponse_disassociatedRepositoryNames' - A list of repository names that have had their association with the
-- template removed.
--
-- 'errors', 'batchDisassociateApprovalRuleTemplateFromRepositoriesResponse_errors' - A list of any errors that might have occurred while attempting to remove
-- the association between the template and the repositories.
newBatchDisassociateApprovalRuleTemplateFromRepositoriesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse
newBatchDisassociateApprovalRuleTemplateFromRepositoriesResponse
  pHttpStatus_ =
    BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse'
      { httpStatus =
          pHttpStatus_,
        disassociatedRepositoryNames =
          Core.mempty,
        errors =
          Core.mempty
      }

-- | The response's http status code.
batchDisassociateApprovalRuleTemplateFromRepositoriesResponse_httpStatus :: Lens.Lens' BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse Core.Int
batchDisassociateApprovalRuleTemplateFromRepositoriesResponse_httpStatus = Lens.lens (\BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse' {httpStatus} -> httpStatus) (\s@BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse' {} a -> s {httpStatus = a} :: BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse)

-- | A list of repository names that have had their association with the
-- template removed.
batchDisassociateApprovalRuleTemplateFromRepositoriesResponse_disassociatedRepositoryNames :: Lens.Lens' BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse [Core.Text]
batchDisassociateApprovalRuleTemplateFromRepositoriesResponse_disassociatedRepositoryNames = Lens.lens (\BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse' {disassociatedRepositoryNames} -> disassociatedRepositoryNames) (\s@BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse' {} a -> s {disassociatedRepositoryNames = a} :: BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse) Core.. Lens._Coerce

-- | A list of any errors that might have occurred while attempting to remove
-- the association between the template and the repositories.
batchDisassociateApprovalRuleTemplateFromRepositoriesResponse_errors :: Lens.Lens' BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse [BatchDisassociateApprovalRuleTemplateFromRepositoriesError]
batchDisassociateApprovalRuleTemplateFromRepositoriesResponse_errors = Lens.lens (\BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse' {errors} -> errors) (\s@BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse' {} a -> s {errors = a} :: BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse) Core.. Lens._Coerce

instance
  Core.NFData
    BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse
