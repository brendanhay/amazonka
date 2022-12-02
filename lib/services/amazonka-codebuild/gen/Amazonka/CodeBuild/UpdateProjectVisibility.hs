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
-- Module      : Amazonka.CodeBuild.UpdateProjectVisibility
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the public visibility for a project. The project\'s build
-- results, logs, and artifacts are available to the general public. For
-- more information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/public-builds.html Public build projects>
-- in the /CodeBuild User Guide/.
--
-- The following should be kept in mind when making your projects public:
--
-- -   All of a project\'s build results, logs, and artifacts, including
--     builds that were run when the project was private, are available to
--     the general public.
--
-- -   All build logs and artifacts are available to the public.
--     Environment variables, source code, and other sensitive information
--     may have been output to the build logs and artifacts. You must be
--     careful about what information is output to the build logs. Some
--     best practice are:
--
--     -   Do not store sensitive values, especially Amazon Web Services
--         access key IDs and secret access keys, in environment variables.
--         We recommend that you use an Amazon EC2 Systems Manager
--         Parameter Store or Secrets Manager to store sensitive values.
--
--     -   Follow
--         <https://docs.aws.amazon.com/codebuild/latest/userguide/webhooks.html#webhook-best-practices Best practices for using webhooks>
--         in the /CodeBuild User Guide/ to limit which entities can
--         trigger a build, and do not store the buildspec in the project
--         itself, to ensure that your webhooks are as secure as possible.
--
-- -   A malicious user can use public builds to distribute malicious
--     artifacts. We recommend that you review all pull requests to verify
--     that the pull request is a legitimate change. We also recommend that
--     you validate any artifacts with their checksums to make sure that
--     the correct artifacts are being downloaded.
module Amazonka.CodeBuild.UpdateProjectVisibility
  ( -- * Creating a Request
    UpdateProjectVisibility (..),
    newUpdateProjectVisibility,

    -- * Request Lenses
    updateProjectVisibility_resourceAccessRole,
    updateProjectVisibility_projectArn,
    updateProjectVisibility_projectVisibility,

    -- * Destructuring the Response
    UpdateProjectVisibilityResponse (..),
    newUpdateProjectVisibilityResponse,

    -- * Response Lenses
    updateProjectVisibilityResponse_projectVisibility,
    updateProjectVisibilityResponse_publicProjectAlias,
    updateProjectVisibilityResponse_projectArn,
    updateProjectVisibilityResponse_httpStatus,
  )
where

import Amazonka.CodeBuild.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateProjectVisibility' smart constructor.
data UpdateProjectVisibility = UpdateProjectVisibility'
  { -- | The ARN of the IAM role that enables CodeBuild to access the CloudWatch
    -- Logs and Amazon S3 artifacts for the project\'s builds.
    resourceAccessRole :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the build project.
    projectArn :: Prelude.Text,
    projectVisibility :: ProjectVisibilityType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProjectVisibility' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceAccessRole', 'updateProjectVisibility_resourceAccessRole' - The ARN of the IAM role that enables CodeBuild to access the CloudWatch
-- Logs and Amazon S3 artifacts for the project\'s builds.
--
-- 'projectArn', 'updateProjectVisibility_projectArn' - The Amazon Resource Name (ARN) of the build project.
--
-- 'projectVisibility', 'updateProjectVisibility_projectVisibility' - Undocumented member.
newUpdateProjectVisibility ::
  -- | 'projectArn'
  Prelude.Text ->
  -- | 'projectVisibility'
  ProjectVisibilityType ->
  UpdateProjectVisibility
newUpdateProjectVisibility
  pProjectArn_
  pProjectVisibility_ =
    UpdateProjectVisibility'
      { resourceAccessRole =
          Prelude.Nothing,
        projectArn = pProjectArn_,
        projectVisibility = pProjectVisibility_
      }

-- | The ARN of the IAM role that enables CodeBuild to access the CloudWatch
-- Logs and Amazon S3 artifacts for the project\'s builds.
updateProjectVisibility_resourceAccessRole :: Lens.Lens' UpdateProjectVisibility (Prelude.Maybe Prelude.Text)
updateProjectVisibility_resourceAccessRole = Lens.lens (\UpdateProjectVisibility' {resourceAccessRole} -> resourceAccessRole) (\s@UpdateProjectVisibility' {} a -> s {resourceAccessRole = a} :: UpdateProjectVisibility)

-- | The Amazon Resource Name (ARN) of the build project.
updateProjectVisibility_projectArn :: Lens.Lens' UpdateProjectVisibility Prelude.Text
updateProjectVisibility_projectArn = Lens.lens (\UpdateProjectVisibility' {projectArn} -> projectArn) (\s@UpdateProjectVisibility' {} a -> s {projectArn = a} :: UpdateProjectVisibility)

-- | Undocumented member.
updateProjectVisibility_projectVisibility :: Lens.Lens' UpdateProjectVisibility ProjectVisibilityType
updateProjectVisibility_projectVisibility = Lens.lens (\UpdateProjectVisibility' {projectVisibility} -> projectVisibility) (\s@UpdateProjectVisibility' {} a -> s {projectVisibility = a} :: UpdateProjectVisibility)

instance Core.AWSRequest UpdateProjectVisibility where
  type
    AWSResponse UpdateProjectVisibility =
      UpdateProjectVisibilityResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateProjectVisibilityResponse'
            Prelude.<$> (x Data..?> "projectVisibility")
            Prelude.<*> (x Data..?> "publicProjectAlias")
            Prelude.<*> (x Data..?> "projectArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateProjectVisibility where
  hashWithSalt _salt UpdateProjectVisibility' {..} =
    _salt `Prelude.hashWithSalt` resourceAccessRole
      `Prelude.hashWithSalt` projectArn
      `Prelude.hashWithSalt` projectVisibility

instance Prelude.NFData UpdateProjectVisibility where
  rnf UpdateProjectVisibility' {..} =
    Prelude.rnf resourceAccessRole
      `Prelude.seq` Prelude.rnf projectArn
      `Prelude.seq` Prelude.rnf projectVisibility

instance Data.ToHeaders UpdateProjectVisibility where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeBuild_20161006.UpdateProjectVisibility" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateProjectVisibility where
  toJSON UpdateProjectVisibility' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("resourceAccessRole" Data..=)
              Prelude.<$> resourceAccessRole,
            Prelude.Just ("projectArn" Data..= projectArn),
            Prelude.Just
              ("projectVisibility" Data..= projectVisibility)
          ]
      )

instance Data.ToPath UpdateProjectVisibility where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateProjectVisibility where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateProjectVisibilityResponse' smart constructor.
data UpdateProjectVisibilityResponse = UpdateProjectVisibilityResponse'
  { projectVisibility :: Prelude.Maybe ProjectVisibilityType,
    -- | Contains the project identifier used with the public build APIs.
    publicProjectAlias :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the build project.
    projectArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProjectVisibilityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'projectVisibility', 'updateProjectVisibilityResponse_projectVisibility' - Undocumented member.
--
-- 'publicProjectAlias', 'updateProjectVisibilityResponse_publicProjectAlias' - Contains the project identifier used with the public build APIs.
--
-- 'projectArn', 'updateProjectVisibilityResponse_projectArn' - The Amazon Resource Name (ARN) of the build project.
--
-- 'httpStatus', 'updateProjectVisibilityResponse_httpStatus' - The response's http status code.
newUpdateProjectVisibilityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateProjectVisibilityResponse
newUpdateProjectVisibilityResponse pHttpStatus_ =
  UpdateProjectVisibilityResponse'
    { projectVisibility =
        Prelude.Nothing,
      publicProjectAlias = Prelude.Nothing,
      projectArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateProjectVisibilityResponse_projectVisibility :: Lens.Lens' UpdateProjectVisibilityResponse (Prelude.Maybe ProjectVisibilityType)
updateProjectVisibilityResponse_projectVisibility = Lens.lens (\UpdateProjectVisibilityResponse' {projectVisibility} -> projectVisibility) (\s@UpdateProjectVisibilityResponse' {} a -> s {projectVisibility = a} :: UpdateProjectVisibilityResponse)

-- | Contains the project identifier used with the public build APIs.
updateProjectVisibilityResponse_publicProjectAlias :: Lens.Lens' UpdateProjectVisibilityResponse (Prelude.Maybe Prelude.Text)
updateProjectVisibilityResponse_publicProjectAlias = Lens.lens (\UpdateProjectVisibilityResponse' {publicProjectAlias} -> publicProjectAlias) (\s@UpdateProjectVisibilityResponse' {} a -> s {publicProjectAlias = a} :: UpdateProjectVisibilityResponse)

-- | The Amazon Resource Name (ARN) of the build project.
updateProjectVisibilityResponse_projectArn :: Lens.Lens' UpdateProjectVisibilityResponse (Prelude.Maybe Prelude.Text)
updateProjectVisibilityResponse_projectArn = Lens.lens (\UpdateProjectVisibilityResponse' {projectArn} -> projectArn) (\s@UpdateProjectVisibilityResponse' {} a -> s {projectArn = a} :: UpdateProjectVisibilityResponse)

-- | The response's http status code.
updateProjectVisibilityResponse_httpStatus :: Lens.Lens' UpdateProjectVisibilityResponse Prelude.Int
updateProjectVisibilityResponse_httpStatus = Lens.lens (\UpdateProjectVisibilityResponse' {httpStatus} -> httpStatus) (\s@UpdateProjectVisibilityResponse' {} a -> s {httpStatus = a} :: UpdateProjectVisibilityResponse)

instance
  Prelude.NFData
    UpdateProjectVisibilityResponse
  where
  rnf UpdateProjectVisibilityResponse' {..} =
    Prelude.rnf projectVisibility
      `Prelude.seq` Prelude.rnf publicProjectAlias
      `Prelude.seq` Prelude.rnf projectArn
      `Prelude.seq` Prelude.rnf httpStatus
