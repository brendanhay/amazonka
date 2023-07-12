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
-- Module      : Amazonka.IoT.DeleteAccountAuditConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores the default settings for Device Defender audits for this
-- account. Any configuration data you entered is deleted and all audit
-- checks are reset to disabled.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DeleteAccountAuditConfiguration>
-- action.
module Amazonka.IoT.DeleteAccountAuditConfiguration
  ( -- * Creating a Request
    DeleteAccountAuditConfiguration (..),
    newDeleteAccountAuditConfiguration,

    -- * Request Lenses
    deleteAccountAuditConfiguration_deleteScheduledAudits,

    -- * Destructuring the Response
    DeleteAccountAuditConfigurationResponse (..),
    newDeleteAccountAuditConfigurationResponse,

    -- * Response Lenses
    deleteAccountAuditConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAccountAuditConfiguration' smart constructor.
data DeleteAccountAuditConfiguration = DeleteAccountAuditConfiguration'
  { -- | If true, all scheduled audits are deleted.
    deleteScheduledAudits :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccountAuditConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteScheduledAudits', 'deleteAccountAuditConfiguration_deleteScheduledAudits' - If true, all scheduled audits are deleted.
newDeleteAccountAuditConfiguration ::
  DeleteAccountAuditConfiguration
newDeleteAccountAuditConfiguration =
  DeleteAccountAuditConfiguration'
    { deleteScheduledAudits =
        Prelude.Nothing
    }

-- | If true, all scheduled audits are deleted.
deleteAccountAuditConfiguration_deleteScheduledAudits :: Lens.Lens' DeleteAccountAuditConfiguration (Prelude.Maybe Prelude.Bool)
deleteAccountAuditConfiguration_deleteScheduledAudits = Lens.lens (\DeleteAccountAuditConfiguration' {deleteScheduledAudits} -> deleteScheduledAudits) (\s@DeleteAccountAuditConfiguration' {} a -> s {deleteScheduledAudits = a} :: DeleteAccountAuditConfiguration)

instance
  Core.AWSRequest
    DeleteAccountAuditConfiguration
  where
  type
    AWSResponse DeleteAccountAuditConfiguration =
      DeleteAccountAuditConfigurationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAccountAuditConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteAccountAuditConfiguration
  where
  hashWithSalt
    _salt
    DeleteAccountAuditConfiguration' {..} =
      _salt `Prelude.hashWithSalt` deleteScheduledAudits

instance
  Prelude.NFData
    DeleteAccountAuditConfiguration
  where
  rnf DeleteAccountAuditConfiguration' {..} =
    Prelude.rnf deleteScheduledAudits

instance
  Data.ToHeaders
    DeleteAccountAuditConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteAccountAuditConfiguration where
  toPath = Prelude.const "/audit/configuration"

instance Data.ToQuery DeleteAccountAuditConfiguration where
  toQuery DeleteAccountAuditConfiguration' {..} =
    Prelude.mconcat
      [ "deleteScheduledAudits"
          Data.=: deleteScheduledAudits
      ]

-- | /See:/ 'newDeleteAccountAuditConfigurationResponse' smart constructor.
data DeleteAccountAuditConfigurationResponse = DeleteAccountAuditConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccountAuditConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAccountAuditConfigurationResponse_httpStatus' - The response's http status code.
newDeleteAccountAuditConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAccountAuditConfigurationResponse
newDeleteAccountAuditConfigurationResponse
  pHttpStatus_ =
    DeleteAccountAuditConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteAccountAuditConfigurationResponse_httpStatus :: Lens.Lens' DeleteAccountAuditConfigurationResponse Prelude.Int
deleteAccountAuditConfigurationResponse_httpStatus = Lens.lens (\DeleteAccountAuditConfigurationResponse' {httpStatus} -> httpStatus) (\s@DeleteAccountAuditConfigurationResponse' {} a -> s {httpStatus = a} :: DeleteAccountAuditConfigurationResponse)

instance
  Prelude.NFData
    DeleteAccountAuditConfigurationResponse
  where
  rnf DeleteAccountAuditConfigurationResponse' {..} =
    Prelude.rnf httpStatus
