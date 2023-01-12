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
-- Module      : Amazonka.Proton.GetEnvironmentTemplateVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get detailed data for a major or minor version of an environment
-- template.
module Amazonka.Proton.GetEnvironmentTemplateVersion
  ( -- * Creating a Request
    GetEnvironmentTemplateVersion (..),
    newGetEnvironmentTemplateVersion,

    -- * Request Lenses
    getEnvironmentTemplateVersion_majorVersion,
    getEnvironmentTemplateVersion_minorVersion,
    getEnvironmentTemplateVersion_templateName,

    -- * Destructuring the Response
    GetEnvironmentTemplateVersionResponse (..),
    newGetEnvironmentTemplateVersionResponse,

    -- * Response Lenses
    getEnvironmentTemplateVersionResponse_httpStatus,
    getEnvironmentTemplateVersionResponse_environmentTemplateVersion,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEnvironmentTemplateVersion' smart constructor.
data GetEnvironmentTemplateVersion = GetEnvironmentTemplateVersion'
  { -- | To get environment template major version detail data, include
    -- @major Version@.
    majorVersion :: Prelude.Text,
    -- | To get environment template minor version detail data, include
    -- @minorVersion@.
    minorVersion :: Prelude.Text,
    -- | The name of the environment template a version of which you want to get
    -- detailed data for.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEnvironmentTemplateVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'majorVersion', 'getEnvironmentTemplateVersion_majorVersion' - To get environment template major version detail data, include
-- @major Version@.
--
-- 'minorVersion', 'getEnvironmentTemplateVersion_minorVersion' - To get environment template minor version detail data, include
-- @minorVersion@.
--
-- 'templateName', 'getEnvironmentTemplateVersion_templateName' - The name of the environment template a version of which you want to get
-- detailed data for.
newGetEnvironmentTemplateVersion ::
  -- | 'majorVersion'
  Prelude.Text ->
  -- | 'minorVersion'
  Prelude.Text ->
  -- | 'templateName'
  Prelude.Text ->
  GetEnvironmentTemplateVersion
newGetEnvironmentTemplateVersion
  pMajorVersion_
  pMinorVersion_
  pTemplateName_ =
    GetEnvironmentTemplateVersion'
      { majorVersion =
          pMajorVersion_,
        minorVersion = pMinorVersion_,
        templateName = pTemplateName_
      }

-- | To get environment template major version detail data, include
-- @major Version@.
getEnvironmentTemplateVersion_majorVersion :: Lens.Lens' GetEnvironmentTemplateVersion Prelude.Text
getEnvironmentTemplateVersion_majorVersion = Lens.lens (\GetEnvironmentTemplateVersion' {majorVersion} -> majorVersion) (\s@GetEnvironmentTemplateVersion' {} a -> s {majorVersion = a} :: GetEnvironmentTemplateVersion)

-- | To get environment template minor version detail data, include
-- @minorVersion@.
getEnvironmentTemplateVersion_minorVersion :: Lens.Lens' GetEnvironmentTemplateVersion Prelude.Text
getEnvironmentTemplateVersion_minorVersion = Lens.lens (\GetEnvironmentTemplateVersion' {minorVersion} -> minorVersion) (\s@GetEnvironmentTemplateVersion' {} a -> s {minorVersion = a} :: GetEnvironmentTemplateVersion)

-- | The name of the environment template a version of which you want to get
-- detailed data for.
getEnvironmentTemplateVersion_templateName :: Lens.Lens' GetEnvironmentTemplateVersion Prelude.Text
getEnvironmentTemplateVersion_templateName = Lens.lens (\GetEnvironmentTemplateVersion' {templateName} -> templateName) (\s@GetEnvironmentTemplateVersion' {} a -> s {templateName = a} :: GetEnvironmentTemplateVersion)

instance
  Core.AWSRequest
    GetEnvironmentTemplateVersion
  where
  type
    AWSResponse GetEnvironmentTemplateVersion =
      GetEnvironmentTemplateVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEnvironmentTemplateVersionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "environmentTemplateVersion")
      )

instance
  Prelude.Hashable
    GetEnvironmentTemplateVersion
  where
  hashWithSalt _salt GetEnvironmentTemplateVersion' {..} =
    _salt `Prelude.hashWithSalt` majorVersion
      `Prelude.hashWithSalt` minorVersion
      `Prelude.hashWithSalt` templateName

instance Prelude.NFData GetEnvironmentTemplateVersion where
  rnf GetEnvironmentTemplateVersion' {..} =
    Prelude.rnf majorVersion
      `Prelude.seq` Prelude.rnf minorVersion
      `Prelude.seq` Prelude.rnf templateName

instance Data.ToHeaders GetEnvironmentTemplateVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.GetEnvironmentTemplateVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetEnvironmentTemplateVersion where
  toJSON GetEnvironmentTemplateVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("majorVersion" Data..= majorVersion),
            Prelude.Just ("minorVersion" Data..= minorVersion),
            Prelude.Just ("templateName" Data..= templateName)
          ]
      )

instance Data.ToPath GetEnvironmentTemplateVersion where
  toPath = Prelude.const "/"

instance Data.ToQuery GetEnvironmentTemplateVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetEnvironmentTemplateVersionResponse' smart constructor.
data GetEnvironmentTemplateVersionResponse = GetEnvironmentTemplateVersionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The detailed data of the requested environment template version.
    environmentTemplateVersion :: EnvironmentTemplateVersion
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEnvironmentTemplateVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getEnvironmentTemplateVersionResponse_httpStatus' - The response's http status code.
--
-- 'environmentTemplateVersion', 'getEnvironmentTemplateVersionResponse_environmentTemplateVersion' - The detailed data of the requested environment template version.
newGetEnvironmentTemplateVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'environmentTemplateVersion'
  EnvironmentTemplateVersion ->
  GetEnvironmentTemplateVersionResponse
newGetEnvironmentTemplateVersionResponse
  pHttpStatus_
  pEnvironmentTemplateVersion_ =
    GetEnvironmentTemplateVersionResponse'
      { httpStatus =
          pHttpStatus_,
        environmentTemplateVersion =
          pEnvironmentTemplateVersion_
      }

-- | The response's http status code.
getEnvironmentTemplateVersionResponse_httpStatus :: Lens.Lens' GetEnvironmentTemplateVersionResponse Prelude.Int
getEnvironmentTemplateVersionResponse_httpStatus = Lens.lens (\GetEnvironmentTemplateVersionResponse' {httpStatus} -> httpStatus) (\s@GetEnvironmentTemplateVersionResponse' {} a -> s {httpStatus = a} :: GetEnvironmentTemplateVersionResponse)

-- | The detailed data of the requested environment template version.
getEnvironmentTemplateVersionResponse_environmentTemplateVersion :: Lens.Lens' GetEnvironmentTemplateVersionResponse EnvironmentTemplateVersion
getEnvironmentTemplateVersionResponse_environmentTemplateVersion = Lens.lens (\GetEnvironmentTemplateVersionResponse' {environmentTemplateVersion} -> environmentTemplateVersion) (\s@GetEnvironmentTemplateVersionResponse' {} a -> s {environmentTemplateVersion = a} :: GetEnvironmentTemplateVersionResponse)

instance
  Prelude.NFData
    GetEnvironmentTemplateVersionResponse
  where
  rnf GetEnvironmentTemplateVersionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf environmentTemplateVersion
