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
-- Module      : Amazonka.CodeDeploy.GetApplicationRevision
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an application revision.
module Amazonka.CodeDeploy.GetApplicationRevision
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
    getApplicationRevisionResponse_applicationName,
    getApplicationRevisionResponse_revision,
    getApplicationRevisionResponse_revisionInfo,
    getApplicationRevisionResponse_httpStatus,
  )
where

import Amazonka.CodeDeploy.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest GetApplicationRevision where
  type
    AWSResponse GetApplicationRevision =
      GetApplicationRevisionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApplicationRevisionResponse'
            Prelude.<$> (x Data..?> "applicationName")
            Prelude.<*> (x Data..?> "revision")
            Prelude.<*> (x Data..?> "revisionInfo")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetApplicationRevision where
  hashWithSalt _salt GetApplicationRevision' {..} =
    _salt `Prelude.hashWithSalt` applicationName
      `Prelude.hashWithSalt` revision

instance Prelude.NFData GetApplicationRevision where
  rnf GetApplicationRevision' {..} =
    Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf revision

instance Data.ToHeaders GetApplicationRevision where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeDeploy_20141006.GetApplicationRevision" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetApplicationRevision where
  toJSON GetApplicationRevision' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("applicationName" Data..= applicationName),
            Prelude.Just ("revision" Data..= revision)
          ]
      )

instance Data.ToPath GetApplicationRevision where
  toPath = Prelude.const "/"

instance Data.ToQuery GetApplicationRevision where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @GetApplicationRevision@ operation.
--
-- /See:/ 'newGetApplicationRevisionResponse' smart constructor.
data GetApplicationRevisionResponse = GetApplicationRevisionResponse'
  { -- | The name of the application that corresponds to the revision.
    applicationName :: Prelude.Maybe Prelude.Text,
    -- | Additional information about the revision, including type and location.
    revision :: Prelude.Maybe RevisionLocation,
    -- | General information about the revision.
    revisionInfo :: Prelude.Maybe GenericRevisionInfo,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetApplicationRevisionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationName', 'getApplicationRevisionResponse_applicationName' - The name of the application that corresponds to the revision.
--
-- 'revision', 'getApplicationRevisionResponse_revision' - Additional information about the revision, including type and location.
--
-- 'revisionInfo', 'getApplicationRevisionResponse_revisionInfo' - General information about the revision.
--
-- 'httpStatus', 'getApplicationRevisionResponse_httpStatus' - The response's http status code.
newGetApplicationRevisionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetApplicationRevisionResponse
newGetApplicationRevisionResponse pHttpStatus_ =
  GetApplicationRevisionResponse'
    { applicationName =
        Prelude.Nothing,
      revision = Prelude.Nothing,
      revisionInfo = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the application that corresponds to the revision.
getApplicationRevisionResponse_applicationName :: Lens.Lens' GetApplicationRevisionResponse (Prelude.Maybe Prelude.Text)
getApplicationRevisionResponse_applicationName = Lens.lens (\GetApplicationRevisionResponse' {applicationName} -> applicationName) (\s@GetApplicationRevisionResponse' {} a -> s {applicationName = a} :: GetApplicationRevisionResponse)

-- | Additional information about the revision, including type and location.
getApplicationRevisionResponse_revision :: Lens.Lens' GetApplicationRevisionResponse (Prelude.Maybe RevisionLocation)
getApplicationRevisionResponse_revision = Lens.lens (\GetApplicationRevisionResponse' {revision} -> revision) (\s@GetApplicationRevisionResponse' {} a -> s {revision = a} :: GetApplicationRevisionResponse)

-- | General information about the revision.
getApplicationRevisionResponse_revisionInfo :: Lens.Lens' GetApplicationRevisionResponse (Prelude.Maybe GenericRevisionInfo)
getApplicationRevisionResponse_revisionInfo = Lens.lens (\GetApplicationRevisionResponse' {revisionInfo} -> revisionInfo) (\s@GetApplicationRevisionResponse' {} a -> s {revisionInfo = a} :: GetApplicationRevisionResponse)

-- | The response's http status code.
getApplicationRevisionResponse_httpStatus :: Lens.Lens' GetApplicationRevisionResponse Prelude.Int
getApplicationRevisionResponse_httpStatus = Lens.lens (\GetApplicationRevisionResponse' {httpStatus} -> httpStatus) (\s@GetApplicationRevisionResponse' {} a -> s {httpStatus = a} :: GetApplicationRevisionResponse)

instance
  Prelude.NFData
    GetApplicationRevisionResponse
  where
  rnf GetApplicationRevisionResponse' {..} =
    Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf revision
      `Prelude.seq` Prelude.rnf revisionInfo
      `Prelude.seq` Prelude.rnf httpStatus
