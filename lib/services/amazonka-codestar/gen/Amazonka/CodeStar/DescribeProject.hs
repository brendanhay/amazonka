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
-- Module      : Amazonka.CodeStar.DescribeProject
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a project and its resources.
module Amazonka.CodeStar.DescribeProject
  ( -- * Creating a Request
    DescribeProject (..),
    newDescribeProject,

    -- * Request Lenses
    describeProject_id,

    -- * Destructuring the Response
    DescribeProjectResponse (..),
    newDescribeProjectResponse,

    -- * Response Lenses
    describeProjectResponse_arn,
    describeProjectResponse_clientRequestToken,
    describeProjectResponse_createdTimeStamp,
    describeProjectResponse_description,
    describeProjectResponse_id,
    describeProjectResponse_name,
    describeProjectResponse_projectTemplateId,
    describeProjectResponse_stackId,
    describeProjectResponse_status,
    describeProjectResponse_httpStatus,
  )
where

import Amazonka.CodeStar.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeProject' smart constructor.
data DescribeProject = DescribeProject'
  { -- | The ID of the project.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'describeProject_id' - The ID of the project.
newDescribeProject ::
  -- | 'id'
  Prelude.Text ->
  DescribeProject
newDescribeProject pId_ = DescribeProject' {id = pId_}

-- | The ID of the project.
describeProject_id :: Lens.Lens' DescribeProject Prelude.Text
describeProject_id = Lens.lens (\DescribeProject' {id} -> id) (\s@DescribeProject' {} a -> s {id = a} :: DescribeProject)

instance Core.AWSRequest DescribeProject where
  type
    AWSResponse DescribeProject =
      DescribeProjectResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProjectResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "clientRequestToken")
            Prelude.<*> (x Data..?> "createdTimeStamp")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "projectTemplateId")
            Prelude.<*> (x Data..?> "stackId")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeProject where
  hashWithSalt _salt DescribeProject' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DescribeProject where
  rnf DescribeProject' {..} = Prelude.rnf id

instance Data.ToHeaders DescribeProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeStar_20170419.DescribeProject" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeProject where
  toJSON DescribeProject' {..} =
    Data.object
      (Prelude.catMaybes [Prelude.Just ("id" Data..= id)])

instance Data.ToPath DescribeProject where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeProject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeProjectResponse' smart constructor.
data DescribeProjectResponse = DescribeProjectResponse'
  { -- | The Amazon Resource Name (ARN) for the project.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A user- or system-generated token that identifies the entity that
    -- requested project creation.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The date and time the project was created, in timestamp format.
    createdTimeStamp :: Prelude.Maybe Data.POSIX,
    -- | The description of the project, if any.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ID of the project.
    id :: Prelude.Maybe Prelude.Text,
    -- | The display name for the project.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ID for the AWS CodeStar project template used to create the project.
    projectTemplateId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the primary stack in AWS CloudFormation used to generate
    -- resources for the project.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | The project creation or deletion status.
    status :: Prelude.Maybe ProjectStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'describeProjectResponse_arn' - The Amazon Resource Name (ARN) for the project.
--
-- 'clientRequestToken', 'describeProjectResponse_clientRequestToken' - A user- or system-generated token that identifies the entity that
-- requested project creation.
--
-- 'createdTimeStamp', 'describeProjectResponse_createdTimeStamp' - The date and time the project was created, in timestamp format.
--
-- 'description', 'describeProjectResponse_description' - The description of the project, if any.
--
-- 'id', 'describeProjectResponse_id' - The ID of the project.
--
-- 'name', 'describeProjectResponse_name' - The display name for the project.
--
-- 'projectTemplateId', 'describeProjectResponse_projectTemplateId' - The ID for the AWS CodeStar project template used to create the project.
--
-- 'stackId', 'describeProjectResponse_stackId' - The ID of the primary stack in AWS CloudFormation used to generate
-- resources for the project.
--
-- 'status', 'describeProjectResponse_status' - The project creation or deletion status.
--
-- 'httpStatus', 'describeProjectResponse_httpStatus' - The response's http status code.
newDescribeProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeProjectResponse
newDescribeProjectResponse pHttpStatus_ =
  DescribeProjectResponse'
    { arn = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      createdTimeStamp = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      projectTemplateId = Prelude.Nothing,
      stackId = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) for the project.
describeProjectResponse_arn :: Lens.Lens' DescribeProjectResponse (Prelude.Maybe Prelude.Text)
describeProjectResponse_arn = Lens.lens (\DescribeProjectResponse' {arn} -> arn) (\s@DescribeProjectResponse' {} a -> s {arn = a} :: DescribeProjectResponse)

-- | A user- or system-generated token that identifies the entity that
-- requested project creation.
describeProjectResponse_clientRequestToken :: Lens.Lens' DescribeProjectResponse (Prelude.Maybe Prelude.Text)
describeProjectResponse_clientRequestToken = Lens.lens (\DescribeProjectResponse' {clientRequestToken} -> clientRequestToken) (\s@DescribeProjectResponse' {} a -> s {clientRequestToken = a} :: DescribeProjectResponse)

-- | The date and time the project was created, in timestamp format.
describeProjectResponse_createdTimeStamp :: Lens.Lens' DescribeProjectResponse (Prelude.Maybe Prelude.UTCTime)
describeProjectResponse_createdTimeStamp = Lens.lens (\DescribeProjectResponse' {createdTimeStamp} -> createdTimeStamp) (\s@DescribeProjectResponse' {} a -> s {createdTimeStamp = a} :: DescribeProjectResponse) Prelude.. Lens.mapping Data._Time

-- | The description of the project, if any.
describeProjectResponse_description :: Lens.Lens' DescribeProjectResponse (Prelude.Maybe Prelude.Text)
describeProjectResponse_description = Lens.lens (\DescribeProjectResponse' {description} -> description) (\s@DescribeProjectResponse' {} a -> s {description = a} :: DescribeProjectResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The ID of the project.
describeProjectResponse_id :: Lens.Lens' DescribeProjectResponse (Prelude.Maybe Prelude.Text)
describeProjectResponse_id = Lens.lens (\DescribeProjectResponse' {id} -> id) (\s@DescribeProjectResponse' {} a -> s {id = a} :: DescribeProjectResponse)

-- | The display name for the project.
describeProjectResponse_name :: Lens.Lens' DescribeProjectResponse (Prelude.Maybe Prelude.Text)
describeProjectResponse_name = Lens.lens (\DescribeProjectResponse' {name} -> name) (\s@DescribeProjectResponse' {} a -> s {name = a} :: DescribeProjectResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The ID for the AWS CodeStar project template used to create the project.
describeProjectResponse_projectTemplateId :: Lens.Lens' DescribeProjectResponse (Prelude.Maybe Prelude.Text)
describeProjectResponse_projectTemplateId = Lens.lens (\DescribeProjectResponse' {projectTemplateId} -> projectTemplateId) (\s@DescribeProjectResponse' {} a -> s {projectTemplateId = a} :: DescribeProjectResponse)

-- | The ID of the primary stack in AWS CloudFormation used to generate
-- resources for the project.
describeProjectResponse_stackId :: Lens.Lens' DescribeProjectResponse (Prelude.Maybe Prelude.Text)
describeProjectResponse_stackId = Lens.lens (\DescribeProjectResponse' {stackId} -> stackId) (\s@DescribeProjectResponse' {} a -> s {stackId = a} :: DescribeProjectResponse)

-- | The project creation or deletion status.
describeProjectResponse_status :: Lens.Lens' DescribeProjectResponse (Prelude.Maybe ProjectStatus)
describeProjectResponse_status = Lens.lens (\DescribeProjectResponse' {status} -> status) (\s@DescribeProjectResponse' {} a -> s {status = a} :: DescribeProjectResponse)

-- | The response's http status code.
describeProjectResponse_httpStatus :: Lens.Lens' DescribeProjectResponse Prelude.Int
describeProjectResponse_httpStatus = Lens.lens (\DescribeProjectResponse' {httpStatus} -> httpStatus) (\s@DescribeProjectResponse' {} a -> s {httpStatus = a} :: DescribeProjectResponse)

instance Prelude.NFData DescribeProjectResponse where
  rnf DescribeProjectResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf createdTimeStamp
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf projectTemplateId
      `Prelude.seq` Prelude.rnf stackId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
