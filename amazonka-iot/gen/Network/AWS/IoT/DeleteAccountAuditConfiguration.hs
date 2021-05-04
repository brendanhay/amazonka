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
-- Module      : Network.AWS.IoT.DeleteAccountAuditConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores the default settings for Device Defender audits for this
-- account. Any configuration data you entered is deleted and all audit
-- checks are reset to disabled.
module Network.AWS.IoT.DeleteAccountAuditConfiguration
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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteAccountAuditConfiguration' smart constructor.
data DeleteAccountAuditConfiguration = DeleteAccountAuditConfiguration'
  { -- | If true, all scheduled audits are deleted.
    deleteScheduledAudits :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.AWSRequest
    DeleteAccountAuditConfiguration
  where
  type
    Rs DeleteAccountAuditConfiguration =
      DeleteAccountAuditConfigurationResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAccountAuditConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteAccountAuditConfiguration

instance
  Prelude.NFData
    DeleteAccountAuditConfiguration

instance
  Prelude.ToHeaders
    DeleteAccountAuditConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    DeleteAccountAuditConfiguration
  where
  toPath = Prelude.const "/audit/configuration"

instance
  Prelude.ToQuery
    DeleteAccountAuditConfiguration
  where
  toQuery DeleteAccountAuditConfiguration' {..} =
    Prelude.mconcat
      [ "deleteScheduledAudits"
          Prelude.=: deleteScheduledAudits
      ]

-- | /See:/ 'newDeleteAccountAuditConfigurationResponse' smart constructor.
data DeleteAccountAuditConfigurationResponse = DeleteAccountAuditConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
