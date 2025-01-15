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
-- Module      : Amazonka.CodeCommit.AssociateApprovalRuleTemplateWithRepository
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an association between an approval rule template and a specified
-- repository. Then, the next time a pull request is created in the
-- repository where the destination reference (if specified) matches the
-- destination reference (branch) for the pull request, an approval rule
-- that matches the template conditions is automatically created for that
-- pull request. If no destination references are specified in the
-- template, an approval rule that matches the template contents is created
-- for all pull requests in that repository.
module Amazonka.CodeCommit.AssociateApprovalRuleTemplateWithRepository
  ( -- * Creating a Request
    AssociateApprovalRuleTemplateWithRepository (..),
    newAssociateApprovalRuleTemplateWithRepository,

    -- * Request Lenses
    associateApprovalRuleTemplateWithRepository_approvalRuleTemplateName,
    associateApprovalRuleTemplateWithRepository_repositoryName,

    -- * Destructuring the Response
    AssociateApprovalRuleTemplateWithRepositoryResponse (..),
    newAssociateApprovalRuleTemplateWithRepositoryResponse,
  )
where

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateApprovalRuleTemplateWithRepository' smart constructor.
data AssociateApprovalRuleTemplateWithRepository = AssociateApprovalRuleTemplateWithRepository'
  { -- | The name for the approval rule template.
    approvalRuleTemplateName :: Prelude.Text,
    -- | The name of the repository that you want to associate with the template.
    repositoryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateApprovalRuleTemplateWithRepository' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'approvalRuleTemplateName', 'associateApprovalRuleTemplateWithRepository_approvalRuleTemplateName' - The name for the approval rule template.
--
-- 'repositoryName', 'associateApprovalRuleTemplateWithRepository_repositoryName' - The name of the repository that you want to associate with the template.
newAssociateApprovalRuleTemplateWithRepository ::
  -- | 'approvalRuleTemplateName'
  Prelude.Text ->
  -- | 'repositoryName'
  Prelude.Text ->
  AssociateApprovalRuleTemplateWithRepository
newAssociateApprovalRuleTemplateWithRepository
  pApprovalRuleTemplateName_
  pRepositoryName_ =
    AssociateApprovalRuleTemplateWithRepository'
      { approvalRuleTemplateName =
          pApprovalRuleTemplateName_,
        repositoryName =
          pRepositoryName_
      }

-- | The name for the approval rule template.
associateApprovalRuleTemplateWithRepository_approvalRuleTemplateName :: Lens.Lens' AssociateApprovalRuleTemplateWithRepository Prelude.Text
associateApprovalRuleTemplateWithRepository_approvalRuleTemplateName = Lens.lens (\AssociateApprovalRuleTemplateWithRepository' {approvalRuleTemplateName} -> approvalRuleTemplateName) (\s@AssociateApprovalRuleTemplateWithRepository' {} a -> s {approvalRuleTemplateName = a} :: AssociateApprovalRuleTemplateWithRepository)

-- | The name of the repository that you want to associate with the template.
associateApprovalRuleTemplateWithRepository_repositoryName :: Lens.Lens' AssociateApprovalRuleTemplateWithRepository Prelude.Text
associateApprovalRuleTemplateWithRepository_repositoryName = Lens.lens (\AssociateApprovalRuleTemplateWithRepository' {repositoryName} -> repositoryName) (\s@AssociateApprovalRuleTemplateWithRepository' {} a -> s {repositoryName = a} :: AssociateApprovalRuleTemplateWithRepository)

instance
  Core.AWSRequest
    AssociateApprovalRuleTemplateWithRepository
  where
  type
    AWSResponse
      AssociateApprovalRuleTemplateWithRepository =
      AssociateApprovalRuleTemplateWithRepositoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      AssociateApprovalRuleTemplateWithRepositoryResponse'

instance
  Prelude.Hashable
    AssociateApprovalRuleTemplateWithRepository
  where
  hashWithSalt
    _salt
    AssociateApprovalRuleTemplateWithRepository' {..} =
      _salt
        `Prelude.hashWithSalt` approvalRuleTemplateName
        `Prelude.hashWithSalt` repositoryName

instance
  Prelude.NFData
    AssociateApprovalRuleTemplateWithRepository
  where
  rnf AssociateApprovalRuleTemplateWithRepository' {..} =
    Prelude.rnf approvalRuleTemplateName `Prelude.seq`
      Prelude.rnf repositoryName

instance
  Data.ToHeaders
    AssociateApprovalRuleTemplateWithRepository
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeCommit_20150413.AssociateApprovalRuleTemplateWithRepository" ::
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
    AssociateApprovalRuleTemplateWithRepository
  where
  toJSON
    AssociateApprovalRuleTemplateWithRepository' {..} =
      Data.object
        ( Prelude.catMaybes
            [ Prelude.Just
                ( "approvalRuleTemplateName"
                    Data..= approvalRuleTemplateName
                ),
              Prelude.Just
                ("repositoryName" Data..= repositoryName)
            ]
        )

instance
  Data.ToPath
    AssociateApprovalRuleTemplateWithRepository
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    AssociateApprovalRuleTemplateWithRepository
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateApprovalRuleTemplateWithRepositoryResponse' smart constructor.
data AssociateApprovalRuleTemplateWithRepositoryResponse = AssociateApprovalRuleTemplateWithRepositoryResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateApprovalRuleTemplateWithRepositoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAssociateApprovalRuleTemplateWithRepositoryResponse ::
  AssociateApprovalRuleTemplateWithRepositoryResponse
newAssociateApprovalRuleTemplateWithRepositoryResponse =
  AssociateApprovalRuleTemplateWithRepositoryResponse'

instance
  Prelude.NFData
    AssociateApprovalRuleTemplateWithRepositoryResponse
  where
  rnf _ = ()
