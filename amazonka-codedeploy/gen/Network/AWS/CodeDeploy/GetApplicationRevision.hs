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
-- Module      : Network.AWS.CodeDeploy.GetApplicationRevision
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an application revision.
module Network.AWS.CodeDeploy.GetApplicationRevision
  ( -- * Creating a Request
    GetApplicationRevision (..),
    newGetApplicationRevision,

    -- * Request Lenses
    getApplicationRevision_applicationName,
    getApplicationRevision_revision,

    -- * Destructuring the Response
    GetApplicationRevisionResponse (..),
    newGetApplicationRevisionResponse,

    -- * Response Lenses
    getApplicationRevisionResponse_revisionInfo,
    getApplicationRevisionResponse_revision,
    getApplicationRevisionResponse_applicationName,
    getApplicationRevisionResponse_httpStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @GetApplicationRevision@ operation.
--
-- /See:/ 'newGetApplicationRevision' smart constructor.
data GetApplicationRevision = GetApplicationRevision'
  { -- | The name of the application that corresponds to the revision.
    applicationName :: Core.Text,
    -- | Information about the application revision to get, including type and
    -- location.
    revision :: RevisionLocation
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetApplicationRevision' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationName', 'getApplicationRevision_applicationName' - The name of the application that corresponds to the revision.
--
-- 'revision', 'getApplicationRevision_revision' - Information about the application revision to get, including type and
-- location.
newGetApplicationRevision ::
  -- | 'applicationName'
  Core.Text ->
  -- | 'revision'
  RevisionLocation ->
  GetApplicationRevision
newGetApplicationRevision
  pApplicationName_
  pRevision_ =
    GetApplicationRevision'
      { applicationName =
          pApplicationName_,
        revision = pRevision_
      }

-- | The name of the application that corresponds to the revision.
getApplicationRevision_applicationName :: Lens.Lens' GetApplicationRevision Core.Text
getApplicationRevision_applicationName = Lens.lens (\GetApplicationRevision' {applicationName} -> applicationName) (\s@GetApplicationRevision' {} a -> s {applicationName = a} :: GetApplicationRevision)

-- | Information about the application revision to get, including type and
-- location.
getApplicationRevision_revision :: Lens.Lens' GetApplicationRevision RevisionLocation
getApplicationRevision_revision = Lens.lens (\GetApplicationRevision' {revision} -> revision) (\s@GetApplicationRevision' {} a -> s {revision = a} :: GetApplicationRevision)

instance Core.AWSRequest GetApplicationRevision where
  type
    AWSResponse GetApplicationRevision =
      GetApplicationRevisionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApplicationRevisionResponse'
            Core.<$> (x Core..?> "revisionInfo")
            Core.<*> (x Core..?> "revision")
            Core.<*> (x Core..?> "applicationName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetApplicationRevision

instance Core.NFData GetApplicationRevision

instance Core.ToHeaders GetApplicationRevision where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeDeploy_20141006.GetApplicationRevision" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetApplicationRevision where
  toJSON GetApplicationRevision' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("applicationName" Core..= applicationName),
            Core.Just ("revision" Core..= revision)
          ]
      )

instance Core.ToPath GetApplicationRevision where
  toPath = Core.const "/"

instance Core.ToQuery GetApplicationRevision where
  toQuery = Core.const Core.mempty

-- | Represents the output of a @GetApplicationRevision@ operation.
--
-- /See:/ 'newGetApplicationRevisionResponse' smart constructor.
data GetApplicationRevisionResponse = GetApplicationRevisionResponse'
  { -- | General information about the revision.
    revisionInfo :: Core.Maybe GenericRevisionInfo,
    -- | Additional information about the revision, including type and location.
    revision :: Core.Maybe RevisionLocation,
    -- | The name of the application that corresponds to the revision.
    applicationName :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetApplicationRevisionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revisionInfo', 'getApplicationRevisionResponse_revisionInfo' - General information about the revision.
--
-- 'revision', 'getApplicationRevisionResponse_revision' - Additional information about the revision, including type and location.
--
-- 'applicationName', 'getApplicationRevisionResponse_applicationName' - The name of the application that corresponds to the revision.
--
-- 'httpStatus', 'getApplicationRevisionResponse_httpStatus' - The response's http status code.
newGetApplicationRevisionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetApplicationRevisionResponse
newGetApplicationRevisionResponse pHttpStatus_ =
  GetApplicationRevisionResponse'
    { revisionInfo =
        Core.Nothing,
      revision = Core.Nothing,
      applicationName = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | General information about the revision.
getApplicationRevisionResponse_revisionInfo :: Lens.Lens' GetApplicationRevisionResponse (Core.Maybe GenericRevisionInfo)
getApplicationRevisionResponse_revisionInfo = Lens.lens (\GetApplicationRevisionResponse' {revisionInfo} -> revisionInfo) (\s@GetApplicationRevisionResponse' {} a -> s {revisionInfo = a} :: GetApplicationRevisionResponse)

-- | Additional information about the revision, including type and location.
getApplicationRevisionResponse_revision :: Lens.Lens' GetApplicationRevisionResponse (Core.Maybe RevisionLocation)
getApplicationRevisionResponse_revision = Lens.lens (\GetApplicationRevisionResponse' {revision} -> revision) (\s@GetApplicationRevisionResponse' {} a -> s {revision = a} :: GetApplicationRevisionResponse)

-- | The name of the application that corresponds to the revision.
getApplicationRevisionResponse_applicationName :: Lens.Lens' GetApplicationRevisionResponse (Core.Maybe Core.Text)
getApplicationRevisionResponse_applicationName = Lens.lens (\GetApplicationRevisionResponse' {applicationName} -> applicationName) (\s@GetApplicationRevisionResponse' {} a -> s {applicationName = a} :: GetApplicationRevisionResponse)

-- | The response's http status code.
getApplicationRevisionResponse_httpStatus :: Lens.Lens' GetApplicationRevisionResponse Core.Int
getApplicationRevisionResponse_httpStatus = Lens.lens (\GetApplicationRevisionResponse' {httpStatus} -> httpStatus) (\s@GetApplicationRevisionResponse' {} a -> s {httpStatus = a} :: GetApplicationRevisionResponse)

instance Core.NFData GetApplicationRevisionResponse
