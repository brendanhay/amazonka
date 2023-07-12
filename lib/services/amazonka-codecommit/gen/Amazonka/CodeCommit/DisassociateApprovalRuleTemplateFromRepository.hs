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
-- Module      : Amazonka.CodeCommit.DisassociateApprovalRuleTemplateFromRepository
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the association between a template and a repository so that
-- approval rules based on the template are not automatically created when
-- pull requests are created in the specified repository. This does not
-- delete any approval rules previously created for pull requests through
-- the template association.
module Amazonka.CodeCommit.DisassociateApprovalRuleTemplateFromRepository
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

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateApprovalRuleTemplateFromRepository' smart constructor.
data DisassociateApprovalRuleTemplateFromRepository = DisassociateApprovalRuleTemplateFromRepository'
  { -- | The name of the approval rule template to disassociate from a specified
    -- repository.
    approvalRuleTemplateName :: Prelude.Text,
    -- | The name of the repository you want to disassociate from the template.
    repositoryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Core.AWSRequest
    DisassociateApprovalRuleTemplateFromRepository
  where
  type
    AWSResponse
      DisassociateApprovalRuleTemplateFromRepository =
      DisassociateApprovalRuleTemplateFromRepositoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DisassociateApprovalRuleTemplateFromRepositoryResponse'

instance
  Prelude.Hashable
    DisassociateApprovalRuleTemplateFromRepository
  where
  hashWithSalt
    _salt
    DisassociateApprovalRuleTemplateFromRepository' {..} =
      _salt
        `Prelude.hashWithSalt` approvalRuleTemplateName
        `Prelude.hashWithSalt` repositoryName

instance
  Prelude.NFData
    DisassociateApprovalRuleTemplateFromRepository
  where
  rnf
    DisassociateApprovalRuleTemplateFromRepository' {..} =
      Prelude.rnf approvalRuleTemplateName
        `Prelude.seq` Prelude.rnf repositoryName

instance
  Data.ToHeaders
    DisassociateApprovalRuleTemplateFromRepository
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeCommit_20150413.DisassociateApprovalRuleTemplateFromRepository" ::
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
    DisassociateApprovalRuleTemplateFromRepository
  where
  toJSON
    DisassociateApprovalRuleTemplateFromRepository' {..} =
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
    DisassociateApprovalRuleTemplateFromRepository
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DisassociateApprovalRuleTemplateFromRepository
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateApprovalRuleTemplateFromRepositoryResponse' smart constructor.
data DisassociateApprovalRuleTemplateFromRepositoryResponse = DisassociateApprovalRuleTemplateFromRepositoryResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf _ = ()
