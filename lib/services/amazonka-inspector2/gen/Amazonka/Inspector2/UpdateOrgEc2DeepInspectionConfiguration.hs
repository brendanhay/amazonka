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
-- Module      : Amazonka.Inspector2.UpdateOrgEc2DeepInspectionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the Amazon Inspector deep inspection custom paths for your
-- organization. You must be an Amazon Inspector delegated administrator to
-- use this API.
module Amazonka.Inspector2.UpdateOrgEc2DeepInspectionConfiguration
  ( -- * Creating a Request
    UpdateOrgEc2DeepInspectionConfiguration (..),
    newUpdateOrgEc2DeepInspectionConfiguration,

    -- * Request Lenses
    updateOrgEc2DeepInspectionConfiguration_orgPackagePaths,

    -- * Destructuring the Response
    UpdateOrgEc2DeepInspectionConfigurationResponse (..),
    newUpdateOrgEc2DeepInspectionConfigurationResponse,

    -- * Response Lenses
    updateOrgEc2DeepInspectionConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateOrgEc2DeepInspectionConfiguration' smart constructor.
data UpdateOrgEc2DeepInspectionConfiguration = UpdateOrgEc2DeepInspectionConfiguration'
  { -- | The Amazon Inspector deep inspection custom paths you are adding for
    -- your organization.
    orgPackagePaths :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateOrgEc2DeepInspectionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'orgPackagePaths', 'updateOrgEc2DeepInspectionConfiguration_orgPackagePaths' - The Amazon Inspector deep inspection custom paths you are adding for
-- your organization.
newUpdateOrgEc2DeepInspectionConfiguration ::
  UpdateOrgEc2DeepInspectionConfiguration
newUpdateOrgEc2DeepInspectionConfiguration =
  UpdateOrgEc2DeepInspectionConfiguration'
    { orgPackagePaths =
        Prelude.mempty
    }

-- | The Amazon Inspector deep inspection custom paths you are adding for
-- your organization.
updateOrgEc2DeepInspectionConfiguration_orgPackagePaths :: Lens.Lens' UpdateOrgEc2DeepInspectionConfiguration [Prelude.Text]
updateOrgEc2DeepInspectionConfiguration_orgPackagePaths = Lens.lens (\UpdateOrgEc2DeepInspectionConfiguration' {orgPackagePaths} -> orgPackagePaths) (\s@UpdateOrgEc2DeepInspectionConfiguration' {} a -> s {orgPackagePaths = a} :: UpdateOrgEc2DeepInspectionConfiguration) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    UpdateOrgEc2DeepInspectionConfiguration
  where
  type
    AWSResponse
      UpdateOrgEc2DeepInspectionConfiguration =
      UpdateOrgEc2DeepInspectionConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateOrgEc2DeepInspectionConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateOrgEc2DeepInspectionConfiguration
  where
  hashWithSalt
    _salt
    UpdateOrgEc2DeepInspectionConfiguration' {..} =
      _salt `Prelude.hashWithSalt` orgPackagePaths

instance
  Prelude.NFData
    UpdateOrgEc2DeepInspectionConfiguration
  where
  rnf UpdateOrgEc2DeepInspectionConfiguration' {..} =
    Prelude.rnf orgPackagePaths

instance
  Data.ToHeaders
    UpdateOrgEc2DeepInspectionConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    UpdateOrgEc2DeepInspectionConfiguration
  where
  toJSON UpdateOrgEc2DeepInspectionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("orgPackagePaths" Data..= orgPackagePaths)
          ]
      )

instance
  Data.ToPath
    UpdateOrgEc2DeepInspectionConfiguration
  where
  toPath =
    Prelude.const
      "/ec2deepinspectionconfiguration/org/update"

instance
  Data.ToQuery
    UpdateOrgEc2DeepInspectionConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateOrgEc2DeepInspectionConfigurationResponse' smart constructor.
data UpdateOrgEc2DeepInspectionConfigurationResponse = UpdateOrgEc2DeepInspectionConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateOrgEc2DeepInspectionConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateOrgEc2DeepInspectionConfigurationResponse_httpStatus' - The response's http status code.
newUpdateOrgEc2DeepInspectionConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateOrgEc2DeepInspectionConfigurationResponse
newUpdateOrgEc2DeepInspectionConfigurationResponse
  pHttpStatus_ =
    UpdateOrgEc2DeepInspectionConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateOrgEc2DeepInspectionConfigurationResponse_httpStatus :: Lens.Lens' UpdateOrgEc2DeepInspectionConfigurationResponse Prelude.Int
updateOrgEc2DeepInspectionConfigurationResponse_httpStatus = Lens.lens (\UpdateOrgEc2DeepInspectionConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateOrgEc2DeepInspectionConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateOrgEc2DeepInspectionConfigurationResponse)

instance
  Prelude.NFData
    UpdateOrgEc2DeepInspectionConfigurationResponse
  where
  rnf
    UpdateOrgEc2DeepInspectionConfigurationResponse' {..} =
      Prelude.rnf httpStatus
