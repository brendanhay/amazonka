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
-- Module      : Amazonka.Mobile.DescribeProject
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about a project in AWS Mobile Hub.
module Amazonka.Mobile.DescribeProject
  ( -- * Creating a Request
    DescribeProject (..),
    newDescribeProject,

    -- * Request Lenses
    describeProject_syncFromResources,
    describeProject_projectId,

    -- * Destructuring the Response
    DescribeProjectResponse (..),
    newDescribeProjectResponse,

    -- * Response Lenses
    describeProjectResponse_details,
    describeProjectResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Mobile.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request structure used to request details about a project.
--
-- /See:/ 'newDescribeProject' smart constructor.
data DescribeProject = DescribeProject'
  { -- | If set to true, causes AWS Mobile Hub to synchronize information from
    -- other services, e.g., update state of AWS CloudFormation stacks in the
    -- AWS Mobile Hub project.
    syncFromResources :: Prelude.Maybe Prelude.Bool,
    -- | Unique project identifier.
    projectId :: Prelude.Text
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
-- 'syncFromResources', 'describeProject_syncFromResources' - If set to true, causes AWS Mobile Hub to synchronize information from
-- other services, e.g., update state of AWS CloudFormation stacks in the
-- AWS Mobile Hub project.
--
-- 'projectId', 'describeProject_projectId' - Unique project identifier.
newDescribeProject ::
  -- | 'projectId'
  Prelude.Text ->
  DescribeProject
newDescribeProject pProjectId_ =
  DescribeProject'
    { syncFromResources =
        Prelude.Nothing,
      projectId = pProjectId_
    }

-- | If set to true, causes AWS Mobile Hub to synchronize information from
-- other services, e.g., update state of AWS CloudFormation stacks in the
-- AWS Mobile Hub project.
describeProject_syncFromResources :: Lens.Lens' DescribeProject (Prelude.Maybe Prelude.Bool)
describeProject_syncFromResources = Lens.lens (\DescribeProject' {syncFromResources} -> syncFromResources) (\s@DescribeProject' {} a -> s {syncFromResources = a} :: DescribeProject)

-- | Unique project identifier.
describeProject_projectId :: Lens.Lens' DescribeProject Prelude.Text
describeProject_projectId = Lens.lens (\DescribeProject' {projectId} -> projectId) (\s@DescribeProject' {} a -> s {projectId = a} :: DescribeProject)

instance Core.AWSRequest DescribeProject where
  type
    AWSResponse DescribeProject =
      DescribeProjectResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProjectResponse'
            Prelude.<$> (x Data..?> "details")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeProject where
  hashWithSalt _salt DescribeProject' {..} =
    _salt
      `Prelude.hashWithSalt` syncFromResources
      `Prelude.hashWithSalt` projectId

instance Prelude.NFData DescribeProject where
  rnf DescribeProject' {..} =
    Prelude.rnf syncFromResources `Prelude.seq`
      Prelude.rnf projectId

instance Data.ToHeaders DescribeProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeProject where
  toPath = Prelude.const "/project"

instance Data.ToQuery DescribeProject where
  toQuery DescribeProject' {..} =
    Prelude.mconcat
      [ "syncFromResources" Data.=: syncFromResources,
        "projectId" Data.=: projectId
      ]

-- | Result structure used for requests of project details.
--
-- /See:/ 'newDescribeProjectResponse' smart constructor.
data DescribeProjectResponse = DescribeProjectResponse'
  { details :: Prelude.Maybe ProjectDetails,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'details', 'describeProjectResponse_details' - Undocumented member.
--
-- 'httpStatus', 'describeProjectResponse_httpStatus' - The response's http status code.
newDescribeProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeProjectResponse
newDescribeProjectResponse pHttpStatus_ =
  DescribeProjectResponse'
    { details = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeProjectResponse_details :: Lens.Lens' DescribeProjectResponse (Prelude.Maybe ProjectDetails)
describeProjectResponse_details = Lens.lens (\DescribeProjectResponse' {details} -> details) (\s@DescribeProjectResponse' {} a -> s {details = a} :: DescribeProjectResponse)

-- | The response's http status code.
describeProjectResponse_httpStatus :: Lens.Lens' DescribeProjectResponse Prelude.Int
describeProjectResponse_httpStatus = Lens.lens (\DescribeProjectResponse' {httpStatus} -> httpStatus) (\s@DescribeProjectResponse' {} a -> s {httpStatus = a} :: DescribeProjectResponse)

instance Prelude.NFData DescribeProjectResponse where
  rnf DescribeProjectResponse' {..} =
    Prelude.rnf details `Prelude.seq`
      Prelude.rnf httpStatus
