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
-- Module      : Amazonka.CodeGuruReviewer.DescribeRepositoryAssociation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_RepositoryAssociation.html RepositoryAssociation>
-- object that contains information about the requested repository
-- association.
module Amazonka.CodeGuruReviewer.DescribeRepositoryAssociation
  ( -- * Creating a Request
    DescribeRepositoryAssociation (..),
    newDescribeRepositoryAssociation,

    -- * Request Lenses
    describeRepositoryAssociation_associationArn,

    -- * Destructuring the Response
    DescribeRepositoryAssociationResponse (..),
    newDescribeRepositoryAssociationResponse,

    -- * Response Lenses
    describeRepositoryAssociationResponse_tags,
    describeRepositoryAssociationResponse_repositoryAssociation,
    describeRepositoryAssociationResponse_httpStatus,
  )
where

import Amazonka.CodeGuruReviewer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeRepositoryAssociation' smart constructor.
data DescribeRepositoryAssociation = DescribeRepositoryAssociation'
  { -- | The Amazon Resource Name (ARN) of the
    -- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_RepositoryAssociation.html RepositoryAssociation>
    -- object. You can retrieve this ARN by calling
    -- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_ListRepositoryAssociations.html ListRepositoryAssociations>.
    associationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRepositoryAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationArn', 'describeRepositoryAssociation_associationArn' - The Amazon Resource Name (ARN) of the
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_RepositoryAssociation.html RepositoryAssociation>
-- object. You can retrieve this ARN by calling
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_ListRepositoryAssociations.html ListRepositoryAssociations>.
newDescribeRepositoryAssociation ::
  -- | 'associationArn'
  Prelude.Text ->
  DescribeRepositoryAssociation
newDescribeRepositoryAssociation pAssociationArn_ =
  DescribeRepositoryAssociation'
    { associationArn =
        pAssociationArn_
    }

-- | The Amazon Resource Name (ARN) of the
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_RepositoryAssociation.html RepositoryAssociation>
-- object. You can retrieve this ARN by calling
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_ListRepositoryAssociations.html ListRepositoryAssociations>.
describeRepositoryAssociation_associationArn :: Lens.Lens' DescribeRepositoryAssociation Prelude.Text
describeRepositoryAssociation_associationArn = Lens.lens (\DescribeRepositoryAssociation' {associationArn} -> associationArn) (\s@DescribeRepositoryAssociation' {} a -> s {associationArn = a} :: DescribeRepositoryAssociation)

instance
  Core.AWSRequest
    DescribeRepositoryAssociation
  where
  type
    AWSResponse DescribeRepositoryAssociation =
      DescribeRepositoryAssociationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRepositoryAssociationResponse'
            Prelude.<$> (x Core..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "RepositoryAssociation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeRepositoryAssociation
  where
  hashWithSalt _salt DescribeRepositoryAssociation' {..} =
    _salt `Prelude.hashWithSalt` associationArn

instance Prelude.NFData DescribeRepositoryAssociation where
  rnf DescribeRepositoryAssociation' {..} =
    Prelude.rnf associationArn

instance Core.ToHeaders DescribeRepositoryAssociation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeRepositoryAssociation where
  toPath DescribeRepositoryAssociation' {..} =
    Prelude.mconcat
      ["/associations/", Core.toBS associationArn]

instance Core.ToQuery DescribeRepositoryAssociation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeRepositoryAssociationResponse' smart constructor.
data DescribeRepositoryAssociationResponse = DescribeRepositoryAssociationResponse'
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
-- Create a value of 'DescribeRepositoryAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'describeRepositoryAssociationResponse_tags' - An array of key-value pairs used to tag an associated repository. A tag
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
-- 'repositoryAssociation', 'describeRepositoryAssociationResponse_repositoryAssociation' - Information about the repository association.
--
-- 'httpStatus', 'describeRepositoryAssociationResponse_httpStatus' - The response's http status code.
newDescribeRepositoryAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeRepositoryAssociationResponse
newDescribeRepositoryAssociationResponse pHttpStatus_ =
  DescribeRepositoryAssociationResponse'
    { tags =
        Prelude.Nothing,
      repositoryAssociation =
        Prelude.Nothing,
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
describeRepositoryAssociationResponse_tags :: Lens.Lens' DescribeRepositoryAssociationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeRepositoryAssociationResponse_tags = Lens.lens (\DescribeRepositoryAssociationResponse' {tags} -> tags) (\s@DescribeRepositoryAssociationResponse' {} a -> s {tags = a} :: DescribeRepositoryAssociationResponse) Prelude.. Lens.mapping Lens.coerced

-- | Information about the repository association.
describeRepositoryAssociationResponse_repositoryAssociation :: Lens.Lens' DescribeRepositoryAssociationResponse (Prelude.Maybe RepositoryAssociation)
describeRepositoryAssociationResponse_repositoryAssociation = Lens.lens (\DescribeRepositoryAssociationResponse' {repositoryAssociation} -> repositoryAssociation) (\s@DescribeRepositoryAssociationResponse' {} a -> s {repositoryAssociation = a} :: DescribeRepositoryAssociationResponse)

-- | The response's http status code.
describeRepositoryAssociationResponse_httpStatus :: Lens.Lens' DescribeRepositoryAssociationResponse Prelude.Int
describeRepositoryAssociationResponse_httpStatus = Lens.lens (\DescribeRepositoryAssociationResponse' {httpStatus} -> httpStatus) (\s@DescribeRepositoryAssociationResponse' {} a -> s {httpStatus = a} :: DescribeRepositoryAssociationResponse)

instance
  Prelude.NFData
    DescribeRepositoryAssociationResponse
  where
  rnf DescribeRepositoryAssociationResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf repositoryAssociation
      `Prelude.seq` Prelude.rnf httpStatus
