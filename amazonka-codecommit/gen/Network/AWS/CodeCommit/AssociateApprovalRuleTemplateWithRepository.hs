{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CodeCommit.AssociateApprovalRuleTemplateWithRepository
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.CodeCommit.AssociateApprovalRuleTemplateWithRepository
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

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateApprovalRuleTemplateWithRepository' smart constructor.
data AssociateApprovalRuleTemplateWithRepository = AssociateApprovalRuleTemplateWithRepository'
  { -- | The name for the approval rule template.
    approvalRuleTemplateName :: Prelude.Text,
    -- | The name of the repository that you want to associate with the template.
    repositoryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.AWSRequest
    AssociateApprovalRuleTemplateWithRepository
  where
  type
    Rs AssociateApprovalRuleTemplateWithRepository =
      AssociateApprovalRuleTemplateWithRepositoryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      AssociateApprovalRuleTemplateWithRepositoryResponse'

instance
  Prelude.Hashable
    AssociateApprovalRuleTemplateWithRepository

instance
  Prelude.NFData
    AssociateApprovalRuleTemplateWithRepository

instance
  Prelude.ToHeaders
    AssociateApprovalRuleTemplateWithRepository
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeCommit_20150413.AssociateApprovalRuleTemplateWithRepository" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    AssociateApprovalRuleTemplateWithRepository
  where
  toJSON
    AssociateApprovalRuleTemplateWithRepository' {..} =
      Prelude.object
        ( Prelude.catMaybes
            [ Prelude.Just
                ( "approvalRuleTemplateName"
                    Prelude..= approvalRuleTemplateName
                ),
              Prelude.Just
                ("repositoryName" Prelude..= repositoryName)
            ]
        )

instance
  Prelude.ToPath
    AssociateApprovalRuleTemplateWithRepository
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    AssociateApprovalRuleTemplateWithRepository
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateApprovalRuleTemplateWithRepositoryResponse' smart constructor.
data AssociateApprovalRuleTemplateWithRepositoryResponse = AssociateApprovalRuleTemplateWithRepositoryResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
