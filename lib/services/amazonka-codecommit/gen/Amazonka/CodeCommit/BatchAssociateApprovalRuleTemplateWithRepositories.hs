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
-- Module      : Amazonka.CodeCommit.BatchAssociateApprovalRuleTemplateWithRepositories
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an association between an approval rule template and one or more
-- specified repositories.
module Amazonka.CodeCommit.BatchAssociateApprovalRuleTemplateWithRepositories
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

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchAssociateApprovalRuleTemplateWithRepositories' smart constructor.
data BatchAssociateApprovalRuleTemplateWithRepositories = BatchAssociateApprovalRuleTemplateWithRepositories'
  { -- | The name of the template you want to associate with one or more
    -- repositories.
    approvalRuleTemplateName :: Prelude.Text,
    -- | The names of the repositories you want to associate with the template.
    --
    -- The length constraint limit is for each string in the array. The array
    -- itself can be empty.
    repositoryNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  BatchAssociateApprovalRuleTemplateWithRepositories
newBatchAssociateApprovalRuleTemplateWithRepositories
  pApprovalRuleTemplateName_ =
    BatchAssociateApprovalRuleTemplateWithRepositories'
      { approvalRuleTemplateName =
          pApprovalRuleTemplateName_,
        repositoryNames =
          Prelude.mempty
      }

-- | The name of the template you want to associate with one or more
-- repositories.
batchAssociateApprovalRuleTemplateWithRepositories_approvalRuleTemplateName :: Lens.Lens' BatchAssociateApprovalRuleTemplateWithRepositories Prelude.Text
batchAssociateApprovalRuleTemplateWithRepositories_approvalRuleTemplateName = Lens.lens (\BatchAssociateApprovalRuleTemplateWithRepositories' {approvalRuleTemplateName} -> approvalRuleTemplateName) (\s@BatchAssociateApprovalRuleTemplateWithRepositories' {} a -> s {approvalRuleTemplateName = a} :: BatchAssociateApprovalRuleTemplateWithRepositories)

-- | The names of the repositories you want to associate with the template.
--
-- The length constraint limit is for each string in the array. The array
-- itself can be empty.
batchAssociateApprovalRuleTemplateWithRepositories_repositoryNames :: Lens.Lens' BatchAssociateApprovalRuleTemplateWithRepositories [Prelude.Text]
batchAssociateApprovalRuleTemplateWithRepositories_repositoryNames = Lens.lens (\BatchAssociateApprovalRuleTemplateWithRepositories' {repositoryNames} -> repositoryNames) (\s@BatchAssociateApprovalRuleTemplateWithRepositories' {} a -> s {repositoryNames = a} :: BatchAssociateApprovalRuleTemplateWithRepositories) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    BatchAssociateApprovalRuleTemplateWithRepositories
  where
  type
    AWSResponse
      BatchAssociateApprovalRuleTemplateWithRepositories =
      BatchAssociateApprovalRuleTemplateWithRepositoriesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchAssociateApprovalRuleTemplateWithRepositoriesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "associatedRepositoryNames"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "errors" Core..!@ Prelude.mempty)
      )

instance
  Prelude.Hashable
    BatchAssociateApprovalRuleTemplateWithRepositories
  where
  hashWithSalt
    _salt
    BatchAssociateApprovalRuleTemplateWithRepositories' {..} =
      _salt
        `Prelude.hashWithSalt` approvalRuleTemplateName
        `Prelude.hashWithSalt` repositoryNames

instance
  Prelude.NFData
    BatchAssociateApprovalRuleTemplateWithRepositories
  where
  rnf
    BatchAssociateApprovalRuleTemplateWithRepositories' {..} =
      Prelude.rnf approvalRuleTemplateName
        `Prelude.seq` Prelude.rnf repositoryNames

instance
  Data.ToHeaders
    BatchAssociateApprovalRuleTemplateWithRepositories
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeCommit_20150413.BatchAssociateApprovalRuleTemplateWithRepositories" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    BatchAssociateApprovalRuleTemplateWithRepositories
  where
  toJSON
    BatchAssociateApprovalRuleTemplateWithRepositories' {..} =
      Data.object
        ( Prelude.catMaybes
            [ Prelude.Just
                ( "approvalRuleTemplateName"
                    Data..= approvalRuleTemplateName
                ),
              Prelude.Just
                ("repositoryNames" Data..= repositoryNames)
            ]
        )

instance
  Data.ToPath
    BatchAssociateApprovalRuleTemplateWithRepositories
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    BatchAssociateApprovalRuleTemplateWithRepositories
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchAssociateApprovalRuleTemplateWithRepositoriesResponse' smart constructor.
data BatchAssociateApprovalRuleTemplateWithRepositoriesResponse = BatchAssociateApprovalRuleTemplateWithRepositoriesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of names of the repositories that have been associated with the
    -- template.
    associatedRepositoryNames :: [Prelude.Text],
    -- | A list of any errors that might have occurred while attempting to create
    -- the association between the template and the repositories.
    errors :: [BatchAssociateApprovalRuleTemplateWithRepositoriesError]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  BatchAssociateApprovalRuleTemplateWithRepositoriesResponse
newBatchAssociateApprovalRuleTemplateWithRepositoriesResponse
  pHttpStatus_ =
    BatchAssociateApprovalRuleTemplateWithRepositoriesResponse'
      { httpStatus =
          pHttpStatus_,
        associatedRepositoryNames =
          Prelude.mempty,
        errors =
          Prelude.mempty
      }

-- | The response's http status code.
batchAssociateApprovalRuleTemplateWithRepositoriesResponse_httpStatus :: Lens.Lens' BatchAssociateApprovalRuleTemplateWithRepositoriesResponse Prelude.Int
batchAssociateApprovalRuleTemplateWithRepositoriesResponse_httpStatus = Lens.lens (\BatchAssociateApprovalRuleTemplateWithRepositoriesResponse' {httpStatus} -> httpStatus) (\s@BatchAssociateApprovalRuleTemplateWithRepositoriesResponse' {} a -> s {httpStatus = a} :: BatchAssociateApprovalRuleTemplateWithRepositoriesResponse)

-- | A list of names of the repositories that have been associated with the
-- template.
batchAssociateApprovalRuleTemplateWithRepositoriesResponse_associatedRepositoryNames :: Lens.Lens' BatchAssociateApprovalRuleTemplateWithRepositoriesResponse [Prelude.Text]
batchAssociateApprovalRuleTemplateWithRepositoriesResponse_associatedRepositoryNames = Lens.lens (\BatchAssociateApprovalRuleTemplateWithRepositoriesResponse' {associatedRepositoryNames} -> associatedRepositoryNames) (\s@BatchAssociateApprovalRuleTemplateWithRepositoriesResponse' {} a -> s {associatedRepositoryNames = a} :: BatchAssociateApprovalRuleTemplateWithRepositoriesResponse) Prelude.. Lens.coerced

-- | A list of any errors that might have occurred while attempting to create
-- the association between the template and the repositories.
batchAssociateApprovalRuleTemplateWithRepositoriesResponse_errors :: Lens.Lens' BatchAssociateApprovalRuleTemplateWithRepositoriesResponse [BatchAssociateApprovalRuleTemplateWithRepositoriesError]
batchAssociateApprovalRuleTemplateWithRepositoriesResponse_errors = Lens.lens (\BatchAssociateApprovalRuleTemplateWithRepositoriesResponse' {errors} -> errors) (\s@BatchAssociateApprovalRuleTemplateWithRepositoriesResponse' {} a -> s {errors = a} :: BatchAssociateApprovalRuleTemplateWithRepositoriesResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    BatchAssociateApprovalRuleTemplateWithRepositoriesResponse
  where
  rnf
    BatchAssociateApprovalRuleTemplateWithRepositoriesResponse' {..} =
      Prelude.rnf httpStatus
        `Prelude.seq` Prelude.rnf associatedRepositoryNames
        `Prelude.seq` Prelude.rnf errors
