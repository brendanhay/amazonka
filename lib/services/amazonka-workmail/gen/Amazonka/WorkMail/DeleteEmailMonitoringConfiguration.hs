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
-- Module      : Amazonka.WorkMail.DeleteEmailMonitoringConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the email monitoring configuration for a specified organization.
module Amazonka.WorkMail.DeleteEmailMonitoringConfiguration
  ( -- * Creating a Request
    DeleteEmailMonitoringConfiguration (..),
    newDeleteEmailMonitoringConfiguration,

    -- * Request Lenses
    deleteEmailMonitoringConfiguration_organizationId,

    -- * Destructuring the Response
    DeleteEmailMonitoringConfigurationResponse (..),
    newDeleteEmailMonitoringConfigurationResponse,

    -- * Response Lenses
    deleteEmailMonitoringConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newDeleteEmailMonitoringConfiguration' smart constructor.
data DeleteEmailMonitoringConfiguration = DeleteEmailMonitoringConfiguration'
  { -- | The ID of the organization from which the email monitoring configuration
    -- is deleted.
    organizationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEmailMonitoringConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'deleteEmailMonitoringConfiguration_organizationId' - The ID of the organization from which the email monitoring configuration
-- is deleted.
newDeleteEmailMonitoringConfiguration ::
  -- | 'organizationId'
  Prelude.Text ->
  DeleteEmailMonitoringConfiguration
newDeleteEmailMonitoringConfiguration
  pOrganizationId_ =
    DeleteEmailMonitoringConfiguration'
      { organizationId =
          pOrganizationId_
      }

-- | The ID of the organization from which the email monitoring configuration
-- is deleted.
deleteEmailMonitoringConfiguration_organizationId :: Lens.Lens' DeleteEmailMonitoringConfiguration Prelude.Text
deleteEmailMonitoringConfiguration_organizationId = Lens.lens (\DeleteEmailMonitoringConfiguration' {organizationId} -> organizationId) (\s@DeleteEmailMonitoringConfiguration' {} a -> s {organizationId = a} :: DeleteEmailMonitoringConfiguration)

instance
  Core.AWSRequest
    DeleteEmailMonitoringConfiguration
  where
  type
    AWSResponse DeleteEmailMonitoringConfiguration =
      DeleteEmailMonitoringConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteEmailMonitoringConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteEmailMonitoringConfiguration
  where
  hashWithSalt
    _salt
    DeleteEmailMonitoringConfiguration' {..} =
      _salt `Prelude.hashWithSalt` organizationId

instance
  Prelude.NFData
    DeleteEmailMonitoringConfiguration
  where
  rnf DeleteEmailMonitoringConfiguration' {..} =
    Prelude.rnf organizationId

instance
  Data.ToHeaders
    DeleteEmailMonitoringConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.DeleteEmailMonitoringConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DeleteEmailMonitoringConfiguration
  where
  toJSON DeleteEmailMonitoringConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Data..= organizationId)
          ]
      )

instance
  Data.ToPath
    DeleteEmailMonitoringConfiguration
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DeleteEmailMonitoringConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteEmailMonitoringConfigurationResponse' smart constructor.
data DeleteEmailMonitoringConfigurationResponse = DeleteEmailMonitoringConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEmailMonitoringConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteEmailMonitoringConfigurationResponse_httpStatus' - The response's http status code.
newDeleteEmailMonitoringConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteEmailMonitoringConfigurationResponse
newDeleteEmailMonitoringConfigurationResponse
  pHttpStatus_ =
    DeleteEmailMonitoringConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteEmailMonitoringConfigurationResponse_httpStatus :: Lens.Lens' DeleteEmailMonitoringConfigurationResponse Prelude.Int
deleteEmailMonitoringConfigurationResponse_httpStatus = Lens.lens (\DeleteEmailMonitoringConfigurationResponse' {httpStatus} -> httpStatus) (\s@DeleteEmailMonitoringConfigurationResponse' {} a -> s {httpStatus = a} :: DeleteEmailMonitoringConfigurationResponse)

instance
  Prelude.NFData
    DeleteEmailMonitoringConfigurationResponse
  where
  rnf DeleteEmailMonitoringConfigurationResponse' {..} =
    Prelude.rnf httpStatus
