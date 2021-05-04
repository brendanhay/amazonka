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
-- Module      : Network.AWS.CodeCommit.DisassociateApprovalRuleTemplateFromRepository
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the association between a template and a repository so that
-- approval rules based on the template are not automatically created when
-- pull requests are created in the specified repository. This does not
-- delete any approval rules previously created for pull requests through
-- the template association.
module Network.AWS.CodeCommit.DisassociateApprovalRuleTemplateFromRepository
  ( -- * Creating a Request
    DisassociateApprovalRuleTemplateFromRepository (..),
    newDisassociateApprovalRuleTemplateFromRepository,

    -- * Request Lenses
    disassociateApprovalRuleTemplateFromRepository_approvalRuleTemplateName,
    disassociateApprovalRuleTemplateFromRepository_repositoryName,

    -- * Destructuring the Response
    DisassociateApprovalRuleTemplateFromRepositoryResponse (..),
    newDisassociateApprovalRuleTemplateFromRepositoryResponse,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateApprovalRuleTemplateFromRepository' smart constructor.
data DisassociateApprovalRuleTemplateFromRepository = DisassociateApprovalRuleTemplateFromRepository'
  { -- | The name of the approval rule template to disassociate from a specified
    -- repository.
    approvalRuleTemplateName :: Prelude.Text,
    -- | The name of the repository you want to disassociate from the template.
    repositoryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisassociateApprovalRuleTemplateFromRepository' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'approvalRuleTemplateName', 'disassociateApprovalRuleTemplateFromRepository_approvalRuleTemplateName' - The name of the approval rule template to disassociate from a specified
-- repository.
--
-- 'repositoryName', 'disassociateApprovalRuleTemplateFromRepository_repositoryName' - The name of the repository you want to disassociate from the template.
newDisassociateApprovalRuleTemplateFromRepository ::
  -- | 'approvalRuleTemplateName'
  Prelude.Text ->
  -- | 'repositoryName'
  Prelude.Text ->
  DisassociateApprovalRuleTemplateFromRepository
newDisassociateApprovalRuleTemplateFromRepository
  pApprovalRuleTemplateName_
  pRepositoryName_ =
    DisassociateApprovalRuleTemplateFromRepository'
      { approvalRuleTemplateName =
          pApprovalRuleTemplateName_,
        repositoryName =
          pRepositoryName_
      }

-- | The name of the approval rule template to disassociate from a specified
-- repository.
disassociateApprovalRuleTemplateFromRepository_approvalRuleTemplateName :: Lens.Lens' DisassociateApprovalRuleTemplateFromRepository Prelude.Text
disassociateApprovalRuleTemplateFromRepository_approvalRuleTemplateName = Lens.lens (\DisassociateApprovalRuleTemplateFromRepository' {approvalRuleTemplateName} -> approvalRuleTemplateName) (\s@DisassociateApprovalRuleTemplateFromRepository' {} a -> s {approvalRuleTemplateName = a} :: DisassociateApprovalRuleTemplateFromRepository)

-- | The name of the repository you want to disassociate from the template.
disassociateApprovalRuleTemplateFromRepository_repositoryName :: Lens.Lens' DisassociateApprovalRuleTemplateFromRepository Prelude.Text
disassociateApprovalRuleTemplateFromRepository_repositoryName = Lens.lens (\DisassociateApprovalRuleTemplateFromRepository' {repositoryName} -> repositoryName) (\s@DisassociateApprovalRuleTemplateFromRepository' {} a -> s {repositoryName = a} :: DisassociateApprovalRuleTemplateFromRepository)

instance
  Prelude.AWSRequest
    DisassociateApprovalRuleTemplateFromRepository
  where
  type
    Rs
      DisassociateApprovalRuleTemplateFromRepository =
      DisassociateApprovalRuleTemplateFromRepositoryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DisassociateApprovalRuleTemplateFromRepositoryResponse'

instance
  Prelude.Hashable
    DisassociateApprovalRuleTemplateFromRepository

instance
  Prelude.NFData
    DisassociateApprovalRuleTemplateFromRepository

instance
  Prelude.ToHeaders
    DisassociateApprovalRuleTemplateFromRepository
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeCommit_20150413.DisassociateApprovalRuleTemplateFromRepository" ::
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
    DisassociateApprovalRuleTemplateFromRepository
  where
  toJSON
    DisassociateApprovalRuleTemplateFromRepository' {..} =
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
    DisassociateApprovalRuleTemplateFromRepository
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DisassociateApprovalRuleTemplateFromRepository
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateApprovalRuleTemplateFromRepositoryResponse' smart constructor.
data DisassociateApprovalRuleTemplateFromRepositoryResponse = DisassociateApprovalRuleTemplateFromRepositoryResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisassociateApprovalRuleTemplateFromRepositoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociateApprovalRuleTemplateFromRepositoryResponse ::
  DisassociateApprovalRuleTemplateFromRepositoryResponse
newDisassociateApprovalRuleTemplateFromRepositoryResponse =
  DisassociateApprovalRuleTemplateFromRepositoryResponse'

instance
  Prelude.NFData
    DisassociateApprovalRuleTemplateFromRepositoryResponse
