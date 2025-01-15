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
-- Module      : Amazonka.LookoutVision.DescribeProject
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an Amazon Lookout for Vision project.
--
-- This operation requires permissions to perform the
-- @lookoutvision:DescribeProject@ operation.
module Amazonka.LookoutVision.DescribeProject
  ( -- * Creating a Request
    DescribeProject (..),
    newDescribeProject,

    -- * Request Lenses
    describeProject_projectName,

    -- * Destructuring the Response
    DescribeProjectResponse (..),
    newDescribeProjectResponse,

    -- * Response Lenses
    describeProjectResponse_projectDescription,
    describeProjectResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutVision.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeProject' smart constructor.
data DescribeProject = DescribeProject'
  { -- | The name of the project that you want to describe.
    projectName :: Prelude.Text
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
-- 'projectName', 'describeProject_projectName' - The name of the project that you want to describe.
newDescribeProject ::
  -- | 'projectName'
  Prelude.Text ->
  DescribeProject
newDescribeProject pProjectName_ =
  DescribeProject' {projectName = pProjectName_}

-- | The name of the project that you want to describe.
describeProject_projectName :: Lens.Lens' DescribeProject Prelude.Text
describeProject_projectName = Lens.lens (\DescribeProject' {projectName} -> projectName) (\s@DescribeProject' {} a -> s {projectName = a} :: DescribeProject)

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
            Prelude.<$> (x Data..?> "ProjectDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeProject where
  hashWithSalt _salt DescribeProject' {..} =
    _salt `Prelude.hashWithSalt` projectName

instance Prelude.NFData DescribeProject where
  rnf DescribeProject' {..} = Prelude.rnf projectName

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
  toPath DescribeProject' {..} =
    Prelude.mconcat
      ["/2020-11-20/projects/", Data.toBS projectName]

instance Data.ToQuery DescribeProject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeProjectResponse' smart constructor.
data DescribeProjectResponse = DescribeProjectResponse'
  { -- | The description of the project.
    projectDescription :: Prelude.Maybe ProjectDescription,
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
-- 'projectDescription', 'describeProjectResponse_projectDescription' - The description of the project.
--
-- 'httpStatus', 'describeProjectResponse_httpStatus' - The response's http status code.
newDescribeProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeProjectResponse
newDescribeProjectResponse pHttpStatus_ =
  DescribeProjectResponse'
    { projectDescription =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The description of the project.
describeProjectResponse_projectDescription :: Lens.Lens' DescribeProjectResponse (Prelude.Maybe ProjectDescription)
describeProjectResponse_projectDescription = Lens.lens (\DescribeProjectResponse' {projectDescription} -> projectDescription) (\s@DescribeProjectResponse' {} a -> s {projectDescription = a} :: DescribeProjectResponse)

-- | The response's http status code.
describeProjectResponse_httpStatus :: Lens.Lens' DescribeProjectResponse Prelude.Int
describeProjectResponse_httpStatus = Lens.lens (\DescribeProjectResponse' {httpStatus} -> httpStatus) (\s@DescribeProjectResponse' {} a -> s {httpStatus = a} :: DescribeProjectResponse)

instance Prelude.NFData DescribeProjectResponse where
  rnf DescribeProjectResponse' {..} =
    Prelude.rnf projectDescription `Prelude.seq`
      Prelude.rnf httpStatus
