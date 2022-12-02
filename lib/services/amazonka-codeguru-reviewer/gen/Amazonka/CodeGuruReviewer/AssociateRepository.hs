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
-- Module      : Amazonka.CodeGuruReviewer.AssociateRepository
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use to associate an Amazon Web Services CodeCommit repository or a
-- repository managed by Amazon Web Services CodeStar Connections with
-- Amazon CodeGuru Reviewer. When you associate a repository, CodeGuru
-- Reviewer reviews source code changes in the repository\'s pull requests
-- and provides automatic recommendations. You can view recommendations
-- using the CodeGuru Reviewer console. For more information, see
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-ug/recommendations.html Recommendations in Amazon CodeGuru Reviewer>
-- in the /Amazon CodeGuru Reviewer User Guide./
--
-- If you associate a CodeCommit or S3 repository, it must be in the same
-- Amazon Web Services Region and Amazon Web Services account where its
-- CodeGuru Reviewer code reviews are configured.
--
-- Bitbucket and GitHub Enterprise Server repositories are managed by
-- Amazon Web Services CodeStar Connections to connect to CodeGuru
-- Reviewer. For more information, see
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-ug/getting-started-associate-repository.html Associate a repository>
-- in the /Amazon CodeGuru Reviewer User Guide./
--
-- You cannot use the CodeGuru Reviewer SDK or the Amazon Web Services CLI
-- to associate a GitHub repository with Amazon CodeGuru Reviewer. To
-- associate a GitHub repository, use the console. For more information,
-- see
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-ug/getting-started-with-guru.html Getting started with CodeGuru Reviewer>
-- in the /CodeGuru Reviewer User Guide./
module Amazonka.CodeGuruReviewer.AssociateRepository
  ( -- * Creating a Request
    AssociateRepository (..),
    newAssociateRepository,

    -- * Request Lenses
    associateRepository_tags,
    associateRepository_clientRequestToken,
    associateRepository_kmsKeyDetails,
    associateRepository_repository,

    -- * Destructuring the Response
    AssociateRepositoryResponse (..),
    newAssociateRepositoryResponse,

    -- * Response Lenses
    associateRepositoryResponse_tags,
    associateRepositoryResponse_repositoryAssociation,
    associateRepositoryResponse_httpStatus,
  )
where

import Amazonka.CodeGuruReviewer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateRepository' smart constructor.
data AssociateRepository = AssociateRepository'
  { -- | An array of key-value pairs used to tag an associated repository. A tag
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
    -- | Amazon CodeGuru Reviewer uses this value to prevent the accidental
    -- creation of duplicate repository associations if there are failures and
    -- retries.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | A @KMSKeyDetails@ object that contains:
    --
    -- -   The encryption option for this repository association. It is either
    --     owned by Amazon Web Services Key Management Service (KMS)
    --     (@AWS_OWNED_CMK@) or customer managed (@CUSTOMER_MANAGED_CMK@).
    --
    -- -   The ID of the Amazon Web Services KMS key that is associated with
    --     this repository association.
    kmsKeyDetails :: Prelude.Maybe KMSKeyDetails,
    -- | The repository to associate.
    repository :: Repository
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateRepository' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'associateRepository_tags' - An array of key-value pairs used to tag an associated repository. A tag
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
-- 'clientRequestToken', 'associateRepository_clientRequestToken' - Amazon CodeGuru Reviewer uses this value to prevent the accidental
-- creation of duplicate repository associations if there are failures and
-- retries.
--
-- 'kmsKeyDetails', 'associateRepository_kmsKeyDetails' - A @KMSKeyDetails@ object that contains:
--
-- -   The encryption option for this repository association. It is either
--     owned by Amazon Web Services Key Management Service (KMS)
--     (@AWS_OWNED_CMK@) or customer managed (@CUSTOMER_MANAGED_CMK@).
--
-- -   The ID of the Amazon Web Services KMS key that is associated with
--     this repository association.
--
-- 'repository', 'associateRepository_repository' - The repository to associate.
newAssociateRepository ::
  -- | 'repository'
  Repository ->
  AssociateRepository
newAssociateRepository pRepository_ =
  AssociateRepository'
    { tags = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      kmsKeyDetails = Prelude.Nothing,
      repository = pRepository_
    }

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
associateRepository_tags :: Lens.Lens' AssociateRepository (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
associateRepository_tags = Lens.lens (\AssociateRepository' {tags} -> tags) (\s@AssociateRepository' {} a -> s {tags = a} :: AssociateRepository) Prelude.. Lens.mapping Lens.coerced

-- | Amazon CodeGuru Reviewer uses this value to prevent the accidental
-- creation of duplicate repository associations if there are failures and
-- retries.
associateRepository_clientRequestToken :: Lens.Lens' AssociateRepository (Prelude.Maybe Prelude.Text)
associateRepository_clientRequestToken = Lens.lens (\AssociateRepository' {clientRequestToken} -> clientRequestToken) (\s@AssociateRepository' {} a -> s {clientRequestToken = a} :: AssociateRepository)

-- | A @KMSKeyDetails@ object that contains:
--
-- -   The encryption option for this repository association. It is either
--     owned by Amazon Web Services Key Management Service (KMS)
--     (@AWS_OWNED_CMK@) or customer managed (@CUSTOMER_MANAGED_CMK@).
--
-- -   The ID of the Amazon Web Services KMS key that is associated with
--     this repository association.
associateRepository_kmsKeyDetails :: Lens.Lens' AssociateRepository (Prelude.Maybe KMSKeyDetails)
associateRepository_kmsKeyDetails = Lens.lens (\AssociateRepository' {kmsKeyDetails} -> kmsKeyDetails) (\s@AssociateRepository' {} a -> s {kmsKeyDetails = a} :: AssociateRepository)

-- | The repository to associate.
associateRepository_repository :: Lens.Lens' AssociateRepository Repository
associateRepository_repository = Lens.lens (\AssociateRepository' {repository} -> repository) (\s@AssociateRepository' {} a -> s {repository = a} :: AssociateRepository)

instance Core.AWSRequest AssociateRepository where
  type
    AWSResponse AssociateRepository =
      AssociateRepositoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateRepositoryResponse'
            Prelude.<$> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "RepositoryAssociation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateRepository where
  hashWithSalt _salt AssociateRepository' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` kmsKeyDetails
      `Prelude.hashWithSalt` repository

instance Prelude.NFData AssociateRepository where
  rnf AssociateRepository' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf kmsKeyDetails
      `Prelude.seq` Prelude.rnf repository

instance Data.ToHeaders AssociateRepository where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateRepository where
  toJSON AssociateRepository' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("KMSKeyDetails" Data..=) Prelude.<$> kmsKeyDetails,
            Prelude.Just ("Repository" Data..= repository)
          ]
      )

instance Data.ToPath AssociateRepository where
  toPath = Prelude.const "/associations"

instance Data.ToQuery AssociateRepository where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateRepositoryResponse' smart constructor.
data AssociateRepositoryResponse = AssociateRepositoryResponse'
  { -- | An array of key-value pairs used to tag an associated repository. A tag
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
    -- | Information about the repository association.
    repositoryAssociation :: Prelude.Maybe RepositoryAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateRepositoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'associateRepositoryResponse_tags' - An array of key-value pairs used to tag an associated repository. A tag
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
-- 'repositoryAssociation', 'associateRepositoryResponse_repositoryAssociation' - Information about the repository association.
--
-- 'httpStatus', 'associateRepositoryResponse_httpStatus' - The response's http status code.
newAssociateRepositoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateRepositoryResponse
newAssociateRepositoryResponse pHttpStatus_ =
  AssociateRepositoryResponse'
    { tags =
        Prelude.Nothing,
      repositoryAssociation = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

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
associateRepositoryResponse_tags :: Lens.Lens' AssociateRepositoryResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
associateRepositoryResponse_tags = Lens.lens (\AssociateRepositoryResponse' {tags} -> tags) (\s@AssociateRepositoryResponse' {} a -> s {tags = a} :: AssociateRepositoryResponse) Prelude.. Lens.mapping Lens.coerced

-- | Information about the repository association.
associateRepositoryResponse_repositoryAssociation :: Lens.Lens' AssociateRepositoryResponse (Prelude.Maybe RepositoryAssociation)
associateRepositoryResponse_repositoryAssociation = Lens.lens (\AssociateRepositoryResponse' {repositoryAssociation} -> repositoryAssociation) (\s@AssociateRepositoryResponse' {} a -> s {repositoryAssociation = a} :: AssociateRepositoryResponse)

-- | The response's http status code.
associateRepositoryResponse_httpStatus :: Lens.Lens' AssociateRepositoryResponse Prelude.Int
associateRepositoryResponse_httpStatus = Lens.lens (\AssociateRepositoryResponse' {httpStatus} -> httpStatus) (\s@AssociateRepositoryResponse' {} a -> s {httpStatus = a} :: AssociateRepositoryResponse)

instance Prelude.NFData AssociateRepositoryResponse where
  rnf AssociateRepositoryResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf repositoryAssociation
      `Prelude.seq` Prelude.rnf httpStatus
