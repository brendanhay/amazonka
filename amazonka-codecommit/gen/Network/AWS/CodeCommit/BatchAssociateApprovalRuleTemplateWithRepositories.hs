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
-- Module      : Network.AWS.CodeCommit.BatchAssociateApprovalRuleTemplateWithRepositories
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an association between an approval rule template and one or more
-- specified repositories.
module Network.AWS.CodeCommit.BatchAssociateApprovalRuleTemplateWithRepositories
  ( -- * Creating a Request
    BatchAssociateApprovalRuleTemplateWithRepositories (..),
    newBatchAssociateApprovalRuleTemplateWithRepositories,

    -- * Request Lenses
    batchAssociateApprovalRuleTemplateWithRepositories_approvalRuleTemplateName,
    batchAssociateApprovalRuleTemplateWithRepositories_repositoryNames,

    -- * Destructuring the Response
    BatchAssociateApprovalRuleTemplateWithRepositoriesResponse (..),
    newBatchAssociateApprovalRuleTemplateWithRepositoriesResponse,

    -- * Response Lenses
    batchAssociateApprovalRuleTemplateWithRepositoriesResponse_httpStatus,
    batchAssociateApprovalRuleTemplateWithRepositoriesResponse_associatedRepositoryNames,
    batchAssociateApprovalRuleTemplateWithRepositoriesResponse_errors,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchAssociateApprovalRuleTemplateWithRepositories' smart constructor.
data BatchAssociateApprovalRuleTemplateWithRepositories = BatchAssociateApprovalRuleTemplateWithRepositories'
  { -- | The name of the template you want to associate with one or more
    -- repositories.
    approvalRuleTemplateName :: Core.Text,
    -- | The names of the repositories you want to associate with the template.
    --
    -- The length constraint limit is for each string in the array. The array
    -- itself can be empty.
    repositoryNames :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchAssociateApprovalRuleTemplateWithRepositories' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'approvalRuleTemplateName', 'batchAssociateApprovalRuleTemplateWithRepositories_approvalRuleTemplateName' - The name of the template you want to associate with one or more
-- repositories.
--
-- 'repositoryNames', 'batchAssociateApprovalRuleTemplateWithRepositories_repositoryNames' - The names of the repositories you want to associate with the template.
--
-- The length constraint limit is for each string in the array. The array
-- itself can be empty.
newBatchAssociateApprovalRuleTemplateWithRepositories ::
  -- | 'approvalRuleTemplateName'
  Core.Text ->
  BatchAssociateApprovalRuleTemplateWithRepositories
newBatchAssociateApprovalRuleTemplateWithRepositories
  pApprovalRuleTemplateName_ =
    BatchAssociateApprovalRuleTemplateWithRepositories'
      { approvalRuleTemplateName =
          pApprovalRuleTemplateName_,
        repositoryNames =
          Core.mempty
      }

-- | The name of the template you want to associate with one or more
-- repositories.
batchAssociateApprovalRuleTemplateWithRepositories_approvalRuleTemplateName :: Lens.Lens' BatchAssociateApprovalRuleTemplateWithRepositories Core.Text
batchAssociateApprovalRuleTemplateWithRepositories_approvalRuleTemplateName = Lens.lens (\BatchAssociateApprovalRuleTemplateWithRepositories' {approvalRuleTemplateName} -> approvalRuleTemplateName) (\s@BatchAssociateApprovalRuleTemplateWithRepositories' {} a -> s {approvalRuleTemplateName = a} :: BatchAssociateApprovalRuleTemplateWithRepositories)

-- | The names of the repositories you want to associate with the template.
--
-- The length constraint limit is for each string in the array. The array
-- itself can be empty.
batchAssociateApprovalRuleTemplateWithRepositories_repositoryNames :: Lens.Lens' BatchAssociateApprovalRuleTemplateWithRepositories [Core.Text]
batchAssociateApprovalRuleTemplateWithRepositories_repositoryNames = Lens.lens (\BatchAssociateApprovalRuleTemplateWithRepositories' {repositoryNames} -> repositoryNames) (\s@BatchAssociateApprovalRuleTemplateWithRepositories' {} a -> s {repositoryNames = a} :: BatchAssociateApprovalRuleTemplateWithRepositories) Core.. Lens._Coerce

instance
  Core.AWSRequest
    BatchAssociateApprovalRuleTemplateWithRepositories
  where
  type
    AWSResponse
      BatchAssociateApprovalRuleTemplateWithRepositories =
      BatchAssociateApprovalRuleTemplateWithRepositoriesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchAssociateApprovalRuleTemplateWithRepositoriesResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
              Core.<*> ( x Core..?> "associatedRepositoryNames"
                           Core..!@ Core.mempty
                       )
              Core.<*> (x Core..?> "errors" Core..!@ Core.mempty)
      )

instance
  Core.Hashable
    BatchAssociateApprovalRuleTemplateWithRepositories

instance
  Core.NFData
    BatchAssociateApprovalRuleTemplateWithRepositories

instance
  Core.ToHeaders
    BatchAssociateApprovalRuleTemplateWithRepositories
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.BatchAssociateApprovalRuleTemplateWithRepositories" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    BatchAssociateApprovalRuleTemplateWithRepositories
  where
  toJSON
    BatchAssociateApprovalRuleTemplateWithRepositories' {..} =
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
    BatchAssociateApprovalRuleTemplateWithRepositories
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    BatchAssociateApprovalRuleTemplateWithRepositories
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newBatchAssociateApprovalRuleTemplateWithRepositoriesResponse' smart constructor.
data BatchAssociateApprovalRuleTemplateWithRepositoriesResponse = BatchAssociateApprovalRuleTemplateWithRepositoriesResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A list of names of the repositories that have been associated with the
    -- template.
    associatedRepositoryNames :: [Core.Text],
    -- | A list of any errors that might have occurred while attempting to create
    -- the association between the template and the repositories.
    errors :: [BatchAssociateApprovalRuleTemplateWithRepositoriesError]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchAssociateApprovalRuleTemplateWithRepositoriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'batchAssociateApprovalRuleTemplateWithRepositoriesResponse_httpStatus' - The response's http status code.
--
-- 'associatedRepositoryNames', 'batchAssociateApprovalRuleTemplateWithRepositoriesResponse_associatedRepositoryNames' - A list of names of the repositories that have been associated with the
-- template.
--
-- 'errors', 'batchAssociateApprovalRuleTemplateWithRepositoriesResponse_errors' - A list of any errors that might have occurred while attempting to create
-- the association between the template and the repositories.
newBatchAssociateApprovalRuleTemplateWithRepositoriesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  BatchAssociateApprovalRuleTemplateWithRepositoriesResponse
newBatchAssociateApprovalRuleTemplateWithRepositoriesResponse
  pHttpStatus_ =
    BatchAssociateApprovalRuleTemplateWithRepositoriesResponse'
      { httpStatus =
          pHttpStatus_,
        associatedRepositoryNames =
          Core.mempty,
        errors =
          Core.mempty
      }

-- | The response's http status code.
batchAssociateApprovalRuleTemplateWithRepositoriesResponse_httpStatus :: Lens.Lens' BatchAssociateApprovalRuleTemplateWithRepositoriesResponse Core.Int
batchAssociateApprovalRuleTemplateWithRepositoriesResponse_httpStatus = Lens.lens (\BatchAssociateApprovalRuleTemplateWithRepositoriesResponse' {httpStatus} -> httpStatus) (\s@BatchAssociateApprovalRuleTemplateWithRepositoriesResponse' {} a -> s {httpStatus = a} :: BatchAssociateApprovalRuleTemplateWithRepositoriesResponse)

-- | A list of names of the repositories that have been associated with the
-- template.
batchAssociateApprovalRuleTemplateWithRepositoriesResponse_associatedRepositoryNames :: Lens.Lens' BatchAssociateApprovalRuleTemplateWithRepositoriesResponse [Core.Text]
batchAssociateApprovalRuleTemplateWithRepositoriesResponse_associatedRepositoryNames = Lens.lens (\BatchAssociateApprovalRuleTemplateWithRepositoriesResponse' {associatedRepositoryNames} -> associatedRepositoryNames) (\s@BatchAssociateApprovalRuleTemplateWithRepositoriesResponse' {} a -> s {associatedRepositoryNames = a} :: BatchAssociateApprovalRuleTemplateWithRepositoriesResponse) Core.. Lens._Coerce

-- | A list of any errors that might have occurred while attempting to create
-- the association between the template and the repositories.
batchAssociateApprovalRuleTemplateWithRepositoriesResponse_errors :: Lens.Lens' BatchAssociateApprovalRuleTemplateWithRepositoriesResponse [BatchAssociateApprovalRuleTemplateWithRepositoriesError]
batchAssociateApprovalRuleTemplateWithRepositoriesResponse_errors = Lens.lens (\BatchAssociateApprovalRuleTemplateWithRepositoriesResponse' {errors} -> errors) (\s@BatchAssociateApprovalRuleTemplateWithRepositoriesResponse' {} a -> s {errors = a} :: BatchAssociateApprovalRuleTemplateWithRepositoriesResponse) Core.. Lens._Coerce

instance
  Core.NFData
    BatchAssociateApprovalRuleTemplateWithRepositoriesResponse
