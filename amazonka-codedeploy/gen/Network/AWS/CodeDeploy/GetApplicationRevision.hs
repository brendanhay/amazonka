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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @GetApplicationRevision@ operation.
--
-- /See:/ 'newGetApplicationRevision' smart constructor.
data GetApplicationRevision = GetApplicationRevision'
  { -- | The name of the application that corresponds to the revision.
    applicationName :: Prelude.Text,
    -- | Information about the application revision to get, including type and
    -- location.
    revision :: RevisionLocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
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
getApplicationRevision_applicationName :: Lens.Lens' GetApplicationRevision Prelude.Text
getApplicationRevision_applicationName = Lens.lens (\GetApplicationRevision' {applicationName} -> applicationName) (\s@GetApplicationRevision' {} a -> s {applicationName = a} :: GetApplicationRevision)

-- | Information about the application revision to get, including type and
-- location.
getApplicationRevision_revision :: Lens.Lens' GetApplicationRevision RevisionLocation
getApplicationRevision_revision = Lens.lens (\GetApplicationRevision' {revision} -> revision) (\s@GetApplicationRevision' {} a -> s {revision = a} :: GetApplicationRevision)

instance Prelude.AWSRequest GetApplicationRevision where
  type
    Rs GetApplicationRevision =
      GetApplicationRevisionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApplicationRevisionResponse'
            Prelude.<$> (x Prelude..?> "revisionInfo")
            Prelude.<*> (x Prelude..?> "revision")
            Prelude.<*> (x Prelude..?> "applicationName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetApplicationRevision

instance Prelude.NFData GetApplicationRevision

instance Prelude.ToHeaders GetApplicationRevision where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeDeploy_20141006.GetApplicationRevision" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetApplicationRevision where
  toJSON GetApplicationRevision' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("applicationName" Prelude..= applicationName),
            Prelude.Just ("revision" Prelude..= revision)
          ]
      )

instance Prelude.ToPath GetApplicationRevision where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetApplicationRevision where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @GetApplicationRevision@ operation.
--
-- /See:/ 'newGetApplicationRevisionResponse' smart constructor.
data GetApplicationRevisionResponse = GetApplicationRevisionResponse'
  { -- | General information about the revision.
    revisionInfo :: Prelude.Maybe GenericRevisionInfo,
    -- | Additional information about the revision, including type and location.
    revision :: Prelude.Maybe RevisionLocation,
    -- | The name of the application that corresponds to the revision.
    applicationName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetApplicationRevisionResponse
newGetApplicationRevisionResponse pHttpStatus_ =
  GetApplicationRevisionResponse'
    { revisionInfo =
        Prelude.Nothing,
      revision = Prelude.Nothing,
      applicationName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | General information about the revision.
getApplicationRevisionResponse_revisionInfo :: Lens.Lens' GetApplicationRevisionResponse (Prelude.Maybe GenericRevisionInfo)
getApplicationRevisionResponse_revisionInfo = Lens.lens (\GetApplicationRevisionResponse' {revisionInfo} -> revisionInfo) (\s@GetApplicationRevisionResponse' {} a -> s {revisionInfo = a} :: GetApplicationRevisionResponse)

-- | Additional information about the revision, including type and location.
getApplicationRevisionResponse_revision :: Lens.Lens' GetApplicationRevisionResponse (Prelude.Maybe RevisionLocation)
getApplicationRevisionResponse_revision = Lens.lens (\GetApplicationRevisionResponse' {revision} -> revision) (\s@GetApplicationRevisionResponse' {} a -> s {revision = a} :: GetApplicationRevisionResponse)

-- | The name of the application that corresponds to the revision.
getApplicationRevisionResponse_applicationName :: Lens.Lens' GetApplicationRevisionResponse (Prelude.Maybe Prelude.Text)
getApplicationRevisionResponse_applicationName = Lens.lens (\GetApplicationRevisionResponse' {applicationName} -> applicationName) (\s@GetApplicationRevisionResponse' {} a -> s {applicationName = a} :: GetApplicationRevisionResponse)

-- | The response's http status code.
getApplicationRevisionResponse_httpStatus :: Lens.Lens' GetApplicationRevisionResponse Prelude.Int
getApplicationRevisionResponse_httpStatus = Lens.lens (\GetApplicationRevisionResponse' {httpStatus} -> httpStatus) (\s@GetApplicationRevisionResponse' {} a -> s {httpStatus = a} :: GetApplicationRevisionResponse)

instance
  Prelude.NFData
    GetApplicationRevisionResponse
