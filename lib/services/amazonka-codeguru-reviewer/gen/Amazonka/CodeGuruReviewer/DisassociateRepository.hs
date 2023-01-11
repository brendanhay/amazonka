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
-- Module      : Amazonka.CodeGuruReviewer.DisassociateRepository
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the association between Amazon CodeGuru Reviewer and a
-- repository.
module Amazonka.CodeGuruReviewer.DisassociateRepository
  ( -- * Creating a Request
    DisassociateRepository (..),
    newDisassociateRepository,

    -- * Request Lenses
    disassociateRepository_associationArn,

    -- * Destructuring the Response
    DisassociateRepositoryResponse (..),
    newDisassociateRepositoryResponse,

    -- * Response Lenses
    disassociateRepositoryResponse_repositoryAssociation,
    disassociateRepositoryResponse_tags,
    disassociateRepositoryResponse_httpStatus,
  )
where

import Amazonka.CodeGuruReviewer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateRepository' smart constructor.
data DisassociateRepository = DisassociateRepository'
  { -- | The Amazon Resource Name (ARN) of the
    -- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_RepositoryAssociation.html RepositoryAssociation>
    -- object. You can retrieve this ARN by calling
    -- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_ListRepositoryAssociations.html ListRepositoryAssociations>.
    associationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateRepository' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationArn', 'disassociateRepository_associationArn' - The Amazon Resource Name (ARN) of the
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_RepositoryAssociation.html RepositoryAssociation>
-- object. You can retrieve this ARN by calling
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_ListRepositoryAssociations.html ListRepositoryAssociations>.
newDisassociateRepository ::
  -- | 'associationArn'
  Prelude.Text ->
  DisassociateRepository
newDisassociateRepository pAssociationArn_ =
  DisassociateRepository'
    { associationArn =
        pAssociationArn_
    }

-- | The Amazon Resource Name (ARN) of the
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_RepositoryAssociation.html RepositoryAssociation>
-- object. You can retrieve this ARN by calling
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_ListRepositoryAssociations.html ListRepositoryAssociations>.
disassociateRepository_associationArn :: Lens.Lens' DisassociateRepository Prelude.Text
disassociateRepository_associationArn = Lens.lens (\DisassociateRepository' {associationArn} -> associationArn) (\s@DisassociateRepository' {} a -> s {associationArn = a} :: DisassociateRepository)

instance Core.AWSRequest DisassociateRepository where
  type
    AWSResponse DisassociateRepository =
      DisassociateRepositoryResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateRepositoryResponse'
            Prelude.<$> (x Data..?> "RepositoryAssociation")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateRepository where
  hashWithSalt _salt DisassociateRepository' {..} =
    _salt `Prelude.hashWithSalt` associationArn

instance Prelude.NFData DisassociateRepository where
  rnf DisassociateRepository' {..} =
    Prelude.rnf associationArn

instance Data.ToHeaders DisassociateRepository where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DisassociateRepository where
  toPath DisassociateRepository' {..} =
    Prelude.mconcat
      ["/associations/", Data.toBS associationArn]

instance Data.ToQuery DisassociateRepository where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateRepositoryResponse' smart constructor.
data DisassociateRepositoryResponse = DisassociateRepositoryResponse'
  { -- | Information about the disassociated repository.
    repositoryAssociation :: Prelude.Maybe RepositoryAssociation,
    -- | An array of key-value pairs used to tag an associated repository. A tag
    -- is a custom attribute label with two parts:
    --
    -- -   A /tag key/ (for example, @CostCenter@, @Environment@, @Project@, or
    --     @Secret@). Tag keys are case sensitive.
    --
    -- -   An optional field known as a /tag value/ (for example,
    --     @111122223333@, @Production@, or a team name). Omitting the tag
    --     value is the same as using an empty string. Like tag keys, tag
    --     values are case sensitive.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateRepositoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoryAssociation', 'disassociateRepositoryResponse_repositoryAssociation' - Information about the disassociated repository.
--
-- 'tags', 'disassociateRepositoryResponse_tags' - An array of key-value pairs used to tag an associated repository. A tag
-- is a custom attribute label with two parts:
--
-- -   A /tag key/ (for example, @CostCenter@, @Environment@, @Project@, or
--     @Secret@). Tag keys are case sensitive.
--
-- -   An optional field known as a /tag value/ (for example,
--     @111122223333@, @Production@, or a team name). Omitting the tag
--     value is the same as using an empty string. Like tag keys, tag
--     values are case sensitive.
--
-- 'httpStatus', 'disassociateRepositoryResponse_httpStatus' - The response's http status code.
newDisassociateRepositoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateRepositoryResponse
newDisassociateRepositoryResponse pHttpStatus_ =
  DisassociateRepositoryResponse'
    { repositoryAssociation =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the disassociated repository.
disassociateRepositoryResponse_repositoryAssociation :: Lens.Lens' DisassociateRepositoryResponse (Prelude.Maybe RepositoryAssociation)
disassociateRepositoryResponse_repositoryAssociation = Lens.lens (\DisassociateRepositoryResponse' {repositoryAssociation} -> repositoryAssociation) (\s@DisassociateRepositoryResponse' {} a -> s {repositoryAssociation = a} :: DisassociateRepositoryResponse)

-- | An array of key-value pairs used to tag an associated repository. A tag
-- is a custom attribute label with two parts:
--
-- -   A /tag key/ (for example, @CostCenter@, @Environment@, @Project@, or
--     @Secret@). Tag keys are case sensitive.
--
-- -   An optional field known as a /tag value/ (for example,
--     @111122223333@, @Production@, or a team name). Omitting the tag
--     value is the same as using an empty string. Like tag keys, tag
--     values are case sensitive.
disassociateRepositoryResponse_tags :: Lens.Lens' DisassociateRepositoryResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
disassociateRepositoryResponse_tags = Lens.lens (\DisassociateRepositoryResponse' {tags} -> tags) (\s@DisassociateRepositoryResponse' {} a -> s {tags = a} :: DisassociateRepositoryResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
disassociateRepositoryResponse_httpStatus :: Lens.Lens' DisassociateRepositoryResponse Prelude.Int
disassociateRepositoryResponse_httpStatus = Lens.lens (\DisassociateRepositoryResponse' {httpStatus} -> httpStatus) (\s@DisassociateRepositoryResponse' {} a -> s {httpStatus = a} :: DisassociateRepositoryResponse)

instance
  Prelude.NFData
    DisassociateRepositoryResponse
  where
  rnf DisassociateRepositoryResponse' {..} =
    Prelude.rnf repositoryAssociation
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
