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
-- Module      : Amazonka.Proton.GetServiceTemplateVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- View detail data for a major or minor version of a service template.
module Amazonka.Proton.GetServiceTemplateVersion
  ( -- * Creating a Request
    GetServiceTemplateVersion (..),
    newGetServiceTemplateVersion,

    -- * Request Lenses
    getServiceTemplateVersion_majorVersion,
    getServiceTemplateVersion_minorVersion,
    getServiceTemplateVersion_templateName,

    -- * Destructuring the Response
    GetServiceTemplateVersionResponse (..),
    newGetServiceTemplateVersionResponse,

    -- * Response Lenses
    getServiceTemplateVersionResponse_httpStatus,
    getServiceTemplateVersionResponse_serviceTemplateVersion,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetServiceTemplateVersion' smart constructor.
data GetServiceTemplateVersion = GetServiceTemplateVersion'
  { -- | To view service template major version detail data, include
    -- @majorVersion@.
    majorVersion :: Prelude.Text,
    -- | To view service template minor version detail data, include
    -- @minorVersion@.
    minorVersion :: Prelude.Text,
    -- | The name of the service template.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceTemplateVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'majorVersion', 'getServiceTemplateVersion_majorVersion' - To view service template major version detail data, include
-- @majorVersion@.
--
-- 'minorVersion', 'getServiceTemplateVersion_minorVersion' - To view service template minor version detail data, include
-- @minorVersion@.
--
-- 'templateName', 'getServiceTemplateVersion_templateName' - The name of the service template.
newGetServiceTemplateVersion ::
  -- | 'majorVersion'
  Prelude.Text ->
  -- | 'minorVersion'
  Prelude.Text ->
  -- | 'templateName'
  Prelude.Text ->
  GetServiceTemplateVersion
newGetServiceTemplateVersion
  pMajorVersion_
  pMinorVersion_
  pTemplateName_ =
    GetServiceTemplateVersion'
      { majorVersion =
          pMajorVersion_,
        minorVersion = pMinorVersion_,
        templateName = pTemplateName_
      }

-- | To view service template major version detail data, include
-- @majorVersion@.
getServiceTemplateVersion_majorVersion :: Lens.Lens' GetServiceTemplateVersion Prelude.Text
getServiceTemplateVersion_majorVersion = Lens.lens (\GetServiceTemplateVersion' {majorVersion} -> majorVersion) (\s@GetServiceTemplateVersion' {} a -> s {majorVersion = a} :: GetServiceTemplateVersion)

-- | To view service template minor version detail data, include
-- @minorVersion@.
getServiceTemplateVersion_minorVersion :: Lens.Lens' GetServiceTemplateVersion Prelude.Text
getServiceTemplateVersion_minorVersion = Lens.lens (\GetServiceTemplateVersion' {minorVersion} -> minorVersion) (\s@GetServiceTemplateVersion' {} a -> s {minorVersion = a} :: GetServiceTemplateVersion)

-- | The name of the service template.
getServiceTemplateVersion_templateName :: Lens.Lens' GetServiceTemplateVersion Prelude.Text
getServiceTemplateVersion_templateName = Lens.lens (\GetServiceTemplateVersion' {templateName} -> templateName) (\s@GetServiceTemplateVersion' {} a -> s {templateName = a} :: GetServiceTemplateVersion)

instance Core.AWSRequest GetServiceTemplateVersion where
  type
    AWSResponse GetServiceTemplateVersion =
      GetServiceTemplateVersionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetServiceTemplateVersionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "serviceTemplateVersion")
      )

instance Prelude.Hashable GetServiceTemplateVersion

instance Prelude.NFData GetServiceTemplateVersion

instance Core.ToHeaders GetServiceTemplateVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AwsProton20200720.GetServiceTemplateVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetServiceTemplateVersion where
  toJSON GetServiceTemplateVersion' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("majorVersion" Core..= majorVersion),
            Prelude.Just ("minorVersion" Core..= minorVersion),
            Prelude.Just ("templateName" Core..= templateName)
          ]
      )

instance Core.ToPath GetServiceTemplateVersion where
  toPath = Prelude.const "/"

instance Core.ToQuery GetServiceTemplateVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetServiceTemplateVersionResponse' smart constructor.
data GetServiceTemplateVersionResponse = GetServiceTemplateVersionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The service template version detail data that\'s returned by AWS Proton.
    serviceTemplateVersion :: ServiceTemplateVersion
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceTemplateVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getServiceTemplateVersionResponse_httpStatus' - The response's http status code.
--
-- 'serviceTemplateVersion', 'getServiceTemplateVersionResponse_serviceTemplateVersion' - The service template version detail data that\'s returned by AWS Proton.
newGetServiceTemplateVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'serviceTemplateVersion'
  ServiceTemplateVersion ->
  GetServiceTemplateVersionResponse
newGetServiceTemplateVersionResponse
  pHttpStatus_
  pServiceTemplateVersion_ =
    GetServiceTemplateVersionResponse'
      { httpStatus =
          pHttpStatus_,
        serviceTemplateVersion =
          pServiceTemplateVersion_
      }

-- | The response's http status code.
getServiceTemplateVersionResponse_httpStatus :: Lens.Lens' GetServiceTemplateVersionResponse Prelude.Int
getServiceTemplateVersionResponse_httpStatus = Lens.lens (\GetServiceTemplateVersionResponse' {httpStatus} -> httpStatus) (\s@GetServiceTemplateVersionResponse' {} a -> s {httpStatus = a} :: GetServiceTemplateVersionResponse)

-- | The service template version detail data that\'s returned by AWS Proton.
getServiceTemplateVersionResponse_serviceTemplateVersion :: Lens.Lens' GetServiceTemplateVersionResponse ServiceTemplateVersion
getServiceTemplateVersionResponse_serviceTemplateVersion = Lens.lens (\GetServiceTemplateVersionResponse' {serviceTemplateVersion} -> serviceTemplateVersion) (\s@GetServiceTemplateVersionResponse' {} a -> s {serviceTemplateVersion = a} :: GetServiceTemplateVersionResponse)

instance
  Prelude.NFData
    GetServiceTemplateVersionResponse
